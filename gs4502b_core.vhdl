-- This CPU is designed to have much higher IPC than a standard 6502 core,
-- through the introduction of a relatively sophisticated and deep pipeline.
-- The objective is that it will be able to dispatch one instruction per cycle
-- under most conditions.  To achieve this it includes an instruction cache so
-- that instructions can be fed quickly, and register/flag renaming logic so
-- that the pipeline stalls as rarely as possible when faced with simple
-- instruction interdependency.
--
-- This approach has some challenges in supporting existing 6502 software,
-- where self-modifying code is common.  In particular, self-modifying code on
-- the 6502 often modifies the very next instruction to be executed, or at
-- least an instruction that will be run within a very few cycles.  The
-- instruction-cache must thus be rapidly updated whenever a memory write
-- occurs.  However, this will likely have a latency of several cycles, which
-- added to the latency of the pipeline, means that we need a way to flush the
-- pipeline whenever self-modifying code is detected that is modifying the
-- currently live instruction stream.  The strategy currently being considered
-- is to stall the execute stage for a sufficient number of cycles to ensure
-- that the pipeline has been flushed, and to allow time for the
-- instruction-cache to be updated. This means we need to detect this before it
-- happens, so that the very next instruction can be invalidated.
--
-- (A special case is if the self-modification only modifies the arguments of an
-- instruction. In that case we can, in theory at least, just change the arg1
-- and arg2 bytes of the instruction in the pipeline.  However, the instruction cache
-- still needs to be invalidated.  Also, for the CHRGET/CHRGOT routine, it
-- doesn't help us, as the instructions that modify the instruction are INC,
-- which is a RMW instruction, and thus we have to wait for the RMW to
-- complete, AND then flush the pipeline while invalidating the cache.
--
-- The real challenge is to work out when self-modification is occurring, so
-- that the pipeline can be flushed and the cache updated.  We have the PC of
-- each instruction as it passes through the pipeline, and we also have the
-- target address of every instruction that writes to memory.  So we can, in
-- theory at least, invalidate an instruction if we have noticed an instruction
-- go through "recently" which would have modified the current instruction, and
-- then trigger a cache miss for that instruction address.  This has the
-- advantage of not requiring every memory write to modify the contents of the
-- instruction-cache, which would pollute the cache, and stop the cache
-- pre-fetch logic from being able to concentrate on populating the cache with
-- the coming instruction stream as quickly as possible, and to be left alone
-- to find consecutive independent instructions that could otherwise be merged.
-- The down-side is that self-modifying code will suffer a noticeable
-- performance hit, as each stalled instruction might cause a delay of
-- somewhere around 16 cycles, while the pipeline flushes out and the
-- cache-miss gets addressed.  Otherwise, it does require that the pipeline
-- keep track of recent write addresses, which adds to the logical complexity.
--
-- But the biggest problem is that if self-modifying code is already loaded
-- into the cache, and modified some distance (in terms of number of
-- instructions) from when the modification occurs, it won't get detected.  It
-- might well be that the only solution to this hazard is to invalidate
-- instruction cache lines that correspond to memory writes, and just put up
-- with the costs it introduces.  A possible trade-off would be to READ the
-- cache lines in question and check if they require invalidation, and then
-- only invalidate them (or better reload them) when this is detected.  Given
-- that only one byte at a time changes, and the other bytes are known from the
-- just-read value, this could be done in a single cycle.  If cached instructions
-- are not modified, then the cache doesn't get invalidated, resulting in
-- better performance, and if they are modified, then they get updated in the
-- most efficient manner possible -- including avoiding a potential cache stall
-- later if only an argument has been updated, which can be patched in the read
-- cache line. In this context we need only keep track of the recent writes to
-- flush the pipeline, and then separately watch memory writes to see if the
-- instruction cache needs updating.
--
-- This approach requires that we can read all 3 potentially tainted cache
-- lines (address, address-1 and address-2) at the same time, to avoid
-- multi-cycle delays on the cache pre-fetch side of things.  This is another
-- argument for having the instruction cache exist as four instruction caches,
-- each corresponding to a different bottom 2 bits of the instruction address.
--
-- This discussion explains why and how we will implement our cache and
-- pipeline management in the face of self-modifying code. It is not yet
-- implemented, and probably won't be until the rest of the CPU is functioning.
-- However, having thought it out, the implementation of the rest of the CPU
-- can proceed, taking the architectural needs of this approach into account.

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.instructions.all;
use work.alu.all;
use work.extra_instruction_equations.all;
use work.visualise.all;

ENTITY gs4502b_core IS
  PORT (
    coreid : in integer;
    cpuclock : IN STD_LOGIC;
    reset : in std_logic;
    
    primary_core_boost : in boolean;

    entity_name : in string;
    
    monitor_PC : out unsigned(15 downto 0);

    fetch_port_read : in fetch_port_out;
    fetch_port_write : out fetch_port_in;
    mem_port_read : in mem_port_out;
    mem_port_write : out mem_port_in;
    
    rom_at_8000 : in std_logic;
    rom_at_a000 : in std_logic;
    rom_at_c000 : in std_logic;
    rom_at_e000 : in std_logic;
    viciii_iomode : in std_logic_vector(1 downto 0)
    );
END gs4502b_core;

architecture behavioural of gs4502b_core is

  signal rom_from_colour_ram : std_logic := '0';

  -- General pipeline control
  signal expected_instruction_address : unsigned(31 downto 0)
    := "00001111000011110000111010101010";
  signal expected_instruction_pch : unsigned(15 downto 8) := "00000000";

  -- Signals output by prefetch stage
  signal stage_prefetch_instruction : instruction_information;
  signal stage_prefetch_instruction_valid : boolean;
  signal branch8_pc : unsigned(15 downto 0);
  signal branch8_zp_pc : unsigned(15 downto 0);
  signal branch16_pc : unsigned(15 downto 0);
  signal prefetch_ready_to_accept_vector_request : boolean;
  
  -- Signals output by decode stage
  signal stage_decode_instruction : instruction_information;
  signal stage_decode_instruction_valid : boolean;
  -- The value we last used, for passing along the pipeline
  signal stage_decode_cache_line_number : unsigned(9 downto 0);
  -- To be wired to cache ram for reading next line
  signal stage_decode_resources_required : instruction_resources;
  signal stage_decode_resources_modified : instruction_resources;
  signal stage_decode_instruction_information : instruction_information;
  signal decode_stalling : boolean;
  -- For requesting the prefetcher read an indirect address vector  
  signal vector_fetch_address : translated_address;
  signal vector_fetch_transaction_id : unsigned(4 downto 0);
  signal vector_fetch_out_transaction_id : unsigned(4 downto 0);
  signal vector_fetch_out_bytes : bytes4;

  -- Signals output by validate stage
  signal validate_stalling : boolean;
  signal stage_validate_instruction : instruction_information;
  signal stage_validate_extra_instruction_flags : extra_instruction_flags;
  signal stage_validate_resources_required : instruction_resources;
  signal stage_validate_resources_modified : instruction_resources;
  signal stage_validate_instruction_valid : boolean;
  signal stage_validate_indirect_ready : boolean;
  signal instruction_address_is_as_expected : boolean;
  signal cache_miss : boolean;
  signal cache_miss_address : translated_address;
  signal cache_miss_pch : unsigned(15 downto 8);
  
  -- Signals output by execute stage
  signal execute_stalling : boolean;
  signal stage_execute_resources_locked : instruction_resources := (others => false);
  signal stage_execute_transaction_id : transaction_id;
  signal stage_execute_transaction_valid : boolean := false;
  signal stage_execute_cpu_personality : cpu_personality := CPU4502;
  signal stage_execute_redirecting : boolean := false;
  signal stage_execute_redirected_address : translated_address;
  signal stage_execute_redirected_pch : unsigned(15 downto 8);
  signal reg_export : cpu_registers;

  -- Signals output by the memory controller
  signal completed_transaction : transaction_result;
  signal memory_stalling : boolean := false;
  
  -- Memory mapping registers and derivatives
  signal reg_mb_low : unsigned(11 downto 0);
  signal reg_offset_low : unsigned(11 downto 0);
  signal reg_map_low : std_logic_vector(3 downto 0);
  signal reg_mb_high : unsigned(19 downto 8);
  signal reg_map_high : std_logic_vector(3 downto 0);
  signal reg_offset_high : unsigned(19 downto 8);
  signal cpuport_value : std_logic_vector(2 downto 0);
  signal cpuport_ddr : std_logic_vector(2 downto 0);
  
begin  -- behavioural
  
  instruction_prefetcher: entity work.gs4502b_instruction_prefetch
    port map (
      cpuclock => cpuclock,
      reset => reset,
      coreid => coreid,
      primary_core_boost => primary_core_boost,
      
      current_cpu_personality => stage_execute_cpu_personality,

      address_redirecting => stage_execute_redirecting,
      redirected_address => stage_execute_redirected_address,
      redirected_pch => stage_execute_redirected_pch,
      stall => decode_stalling,

      vector_fetch_address_in => vector_fetch_address,
      vector_fetch_transaction_id_in => vector_fetch_transaction_id,

      vector_fetch_out_transaction_id => vector_fetch_out_transaction_id,
      vector_fetch_out_bytes => vector_fetch_out_bytes,
      
      regs => reg_export,

      prefetch_ready_to_accept_vector_request => prefetch_ready_to_accept_vector_request,
      
      instruction_out => stage_prefetch_instruction,
      instruction_out_valid => stage_prefetch_instruction_valid,
      branch8_pc => branch8_pc,
      branch8_zp_pc => branch8_zp_pc,
      branch16_pc => branch16_pc,

      fetch_port_read => fetch_port_read,
      fetch_port_write => fetch_port_write
      
      );  
  
  to_stop_ghdl_bug: block
  begin
  decode_stage: entity work.gs4502b_stage_decode
    port map (
      cpuclock => cpuclock,
      coreid => coreid,
      current_cpu_personality => stage_execute_cpu_personality,
      cpuport_value => cpuport_value,
      cpuport_ddr => cpuport_ddr,
      viciii_iomode => viciii_iomode,
      reg_map_low => reg_map_low,
      reg_mb_low => reg_mb_low,
      reg_offset_low => reg_offset_low,
      reg_map_high => reg_map_high,
      reg_mb_high => reg_mb_high,
      reg_offset_high => reg_offset_high,
      rom_at_c000 => rom_at_c000,
      rom_at_e000 => rom_at_e000,
      rom_at_8000 => rom_at_8000,
      rom_at_a000 => rom_at_a000,

      regs => reg_export,

      instruction_in => stage_prefetch_instruction,
      instruction_in_valid => stage_prefetch_instruction_valid,
      branch8_pc => branch8_pc,
      branch8_zp_pc => branch8_zp_pc,
      branch16_pc => branch16_pc,

      instruction_out => stage_decode_instruction,
      instruction_out_valid => stage_decode_instruction_valid,
      
      vector_fetch_address => vector_fetch_address,
      vector_fetch_transaction_id => vector_fetch_transaction_id,
      prefetch_ready_to_accept_vector_request => prefetch_ready_to_accept_vector_request,
      indirect_ready => stage_validate_indirect_ready,
      
      address_redirecting => stage_execute_redirecting,
      redirected_address => stage_execute_redirected_address,
      stall => validate_stalling,
      stalling => decode_stalling

      );
  end block;

  also_to_stop_ghdl_bug: block
  begin
  validate_stage: entity work.gs4502b_stage_validate
    port map (
      cpuclock => cpuclock,
      coreid => coreid,

      stall => execute_stalling,
      resources_freshly_locked_by_execute_stage
      => stage_execute_resources_locked,
      resource_lock_transaction_id_in => stage_execute_transaction_id,
      resource_lock_transaction_valid_in => stage_execute_transaction_valid,
      current_cpu_personality => stage_execute_cpu_personality,
      address_redirecting => stage_execute_redirecting,      
      redirected_address => stage_execute_redirected_address,      
      redirected_pch => stage_execute_redirected_pch,

      completed_transaction => completed_transaction,      

      resources_required_in => stage_decode_resources_required,
      resources_modified_in => stage_decode_resources_modified,

      regs => reg_export,

      instruction_in => stage_decode_instruction,
      instruction_in_valid => stage_decode_instruction_valid,

      vector_fetch_transaction_id => vector_fetch_out_transaction_id,
      vector_fetch_vector => vector_fetch_out_bytes,
      
      instruction_out => stage_validate_instruction,
      instruction_out_extra_flags => stage_validate_extra_instruction_flags,

      instruction_valid => stage_validate_instruction_valid,      
      instruction_address_is_as_expected => instruction_address_is_as_expected,

      indirect_ready => stage_validate_indirect_ready,
      
      resources_required_out => stage_validate_resources_required,
      resources_modified_out => stage_validate_resources_modified,
      stalling => validate_stalling

      );
  end block;

  and_this_one_too: block
  begin
  execute_stage: entity work.gs4502b_stage_execute
    port map (
      cpuclock => cpuclock,
      reset => reset,
      coreid => coreid,

      reg_map_low => reg_map_low,
      reg_mb_low => reg_mb_low,
      reg_offset_low => reg_offset_low,
      reg_map_high => reg_map_high,
      reg_mb_high => reg_mb_high,
      reg_offset_high => reg_offset_high,
      cpuport_ddr => cpuport_ddr,
      cpuport_value => cpuport_value,

      monitor_pc => monitor_pc,

      reg_export => reg_export,
      
      stall => memory_stalling,
      instruction_in => stage_validate_instruction,
      instruction_in_extra_flags => stage_validate_extra_instruction_flags,
      instruction_valid => stage_validate_instruction_valid,
      instruction_address_is_as_expected => instruction_address_is_as_expected,
      
      resources_locked => stage_execute_resources_locked,
      resource_lock_transaction_id_out => stage_execute_transaction_id,
      resource_lock_transaction_valid_out => stage_execute_transaction_valid,
      current_cpu_personality => stage_execute_cpu_personality,
      address_redirecting => stage_execute_redirecting,      
      redirected_address => stage_execute_redirected_address,      
      redirected_pch => stage_execute_redirected_pch,

      completed_transaction => completed_transaction,
      
      stalling => execute_stalling
      );
  end block;

  process (cpuclock) is
    variable ignored : boolean;
  begin
    if rising_edge(cpuclock) then
      ignored := visualise(entity_name,"reset",reset);
      ignored := visualise(entity_name,"primary_core_boost",primary_core_boost);
      ignored := visualise(entity_name,"fetch_port_read",fetch_port_read);
      ignored := visualise(entity_name,"mem_port_read",mem_port_read);
      ignored := visualise(entity_name,"rom_at_8000",rom_at_8000);
      ignored := visualise(entity_name,"rom_at_a000",rom_at_a000);
      ignored := visualise(entity_name,"rom_at_c000",rom_at_c000);
      ignored := visualise(entity_name,"rom_at_e000",rom_at_e000);
      ignored := visualise(entity_name,"viciii_iomode",viciii_iomode);
      ignored := visualise(entity_name,"rom_from_colour_ram",rom_from_colour_ram);
      ignored := visualise(entity_name,"expected_instruction_address",expected_instruction_address);
      ignored := visualise(entity_name,"expected_instruction_pch",expected_instruction_pch);
      ignored := visualise(entity_name,"stage_prefetch_instruction",stage_prefetch_instruction);
      ignored := visualise(entity_name,"stage_prefetch_instruction_valid",stage_prefetch_instruction_valid);
      ignored := visualise(entity_name,"branch8_pc",branch8_pc);
      ignored := visualise(entity_name,"branch8_zp_pc",branch8_zp_pc);
      ignored := visualise(entity_name,"branch16_pc",branch16_pc);
      ignored := visualise(entity_name,"prefetch_ready_to_accept_vector_request",prefetch_ready_to_accept_vector_request);
      ignored := visualise(entity_name,"stage_decode_instruction",stage_decode_instruction);
      ignored := visualise(entity_name,"stage_decode_instruction_valid",stage_decode_instruction_valid);
      ignored := visualise(entity_name,"stage_decode_cache_line_number",stage_decode_cache_line_number);
      ignored := visualise(entity_name,"stage_decode_resources_required",stage_decode_resources_required);
      ignored := visualise(entity_name,"stage_decode_resources_modified",stage_decode_resources_modified);
      ignored := visualise(entity_name,"stage_decode_instruction_information",stage_decode_instruction_information);
      ignored := visualise(entity_name,"decode_stalling",decode_stalling);
      ignored := visualise(entity_name,"vector_fetch_address",vector_fetch_address);
      ignored := visualise(entity_name,"vector_fetch_transaction_id",vector_fetch_transaction_id);
      ignored := visualise(entity_name,"vector_fetch_out_transaction_id",vector_fetch_out_transaction_id);
      ignored := visualise(entity_name,"vector_fetch_out_bytes",vector_fetch_out_bytes);
      ignored := visualise(entity_name,"validate_stalling",validate_stalling);
      ignored := visualise(entity_name,"stage_validate_instruction",stage_validate_instruction);
      ignored := visualise(entity_name,"stage_validate_extra_instruction_flags",stage_validate_extra_instruction_flags);
      ignored := visualise(entity_name,"stage_validate_resources_required",stage_validate_resources_required);
      ignored := visualise(entity_name,"stage_validate_resources_modified",stage_validate_resources_modified);
      ignored := visualise(entity_name,"stage_validate_instruction_valid",stage_validate_instruction_valid);
      ignored := visualise(entity_name,"stage_validate_indirect_ready",stage_validate_indirect_ready);
      ignored := visualise(entity_name,"instruction_address_is_as_expected",instruction_address_is_as_expected);
      ignored := visualise(entity_name,"cache_miss",cache_miss);
      ignored := visualise(entity_name,"cache_miss_address",cache_miss_address);
      ignored := visualise(entity_name,"cache_miss_pch",cache_miss_pch);
      ignored := visualise(entity_name,"execute_stalling",execute_stalling);
      ignored := visualise(entity_name,"stage_execute_resources_locked",stage_execute_resources_locked);
      ignored := visualise(entity_name,"stage_execute_transaction_id",stage_execute_transaction_id);
      ignored := visualise(entity_name,"stage_execute_transaction_valid",stage_execute_transaction_valid);
      ignored := visualise(entity_name,"stage_execute_cpu_personality",stage_execute_cpu_personality);
      ignored := visualise(entity_name,"stage_execute_redirecting",stage_execute_redirecting);
      ignored := visualise(entity_name,"stage_execute_redirected_address",stage_execute_redirected_address);
      ignored := visualise(entity_name,"stage_execute_redirected_pch",stage_execute_redirected_pch);
      ignored := visualise(entity_name,"reg_export",reg_export);
      ignored := visualise(entity_name,"completed_transaction",completed_transaction);
      ignored := visualise(entity_name,"memory_stalling",memory_stalling);
      ignored := visualise(entity_name,"reg_mb_low",reg_mb_low);
      ignored := visualise(entity_name,"reg_offset_low",reg_offset_low);
      ignored := visualise(entity_name,"reg_map_low",reg_map_low);
      ignored := visualise(entity_name,"reg_mb_high",reg_mb_high);
      ignored := visualise(entity_name,"reg_map_high",reg_map_high);
      ignored := visualise(entity_name,"reg_offset_high",reg_offset_high);
      ignored := visualise(entity_name,"cpuport_value",cpuport_value);
      ignored := visualise(entity_name,"cpuport_ddr",cpuport_ddr);
    end if;
  end process;

  
end behavioural;
