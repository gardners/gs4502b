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

ENTITY gs4502b IS
  PORT (
    cpuclock : IN STD_LOGIC;
    reset : in std_logic;
    monitor_PC : out unsigned(15 downto 0);

    rom_at_8000 : in std_logic;
    rom_at_a000 : in std_logic;
    rom_at_c000 : in std_logic;
    rom_at_e000 : in std_logic;
    viciii_iomode : in std_logic_vector(1 downto 0)
    );
END gs4502b;

architecture behavioural of gs4502b is

  signal rom_from_colour_ram : std_logic := '0';

  -- General pipeline control
  signal instruction_ready : std_logic := '0';
  signal expected_instruction_address : unsigned(31 downto 0)
    := "00001111000011110000111010101010";
  signal expected_instruction_pch : unsigned(15 downto 8) := "00000000";

  -- Signals output by prefetch stage
  signal stage_prefetch_instruction : instruction_information;
  
  -- Signals output by decode stage
  signal stage_decode_instruction : instruction_information;
  -- The value we last used, for passing along the pipeline
  signal stage_decode_cache_line_number : unsigned(9 downto 0);
  -- To be wired to cache ram for reading next line
  signal stage_decode_resources_required : instruction_resources;
  signal stage_decode_resources_modified : instruction_resources;
  signal stage_decode_instruction_information : instruction_information;
  signal decode_stalling : boolean;

  -- Signals output by validate stage
  signal validate_stalling : boolean;
  signal stage_validate_instruction : instruction_information;
  signal stage_validate_resources_required : instruction_resources;
  signal stage_validate_resources_modified : instruction_resources;
  signal stage_validate_instruction_valid : boolean;
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

  -- 4x 64KB x 8bit RAMs to make main memory
  -- (Actually, they are 9-bit RAMs.  We aren't currently doing anything with
  -- the 9th bit, but might, for example, us it to mark branch prediction
  -- information.)
  signal ram_instruction_port_address : std_logic_vector(15 downto 0);
  signal ram_instruction_port_data0 : std_logic_vector(8 downto 0);
  signal ram_instruction_port_data1 : std_logic_vector(8 downto 0);
  signal ram_instruction_port_data2 : std_logic_vector(8 downto 0);
  signal ram_instruction_port_data3 : std_logic_vector(8 downto 0);
  signal mc_address : std_logic_vector(15 downto 0);
  signal mc_write : std_logic_vector(3 downto 0) := (others => '0');
  signal mc_write_data : std_logic_vector(8 downto 0) := (others => '0');
  signal mc_read_data : std_logic_vector(8 downto 0);
  
begin  -- behavioural

  ram0: entity work.ram0
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_instruction_port_address,
               a_din => (others => '0'),
               a_dout => ram_instruction_port_data0,

               b_clk => cpuclock,
               b_wr => mc_write(0),
               b_addr => mc_address,
               b_din => mc_write_data,
               b_dout => mc_read_data
               );
  
  ram1: entity work.ram1
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_instruction_port_address,
               a_din => (others => '0'),
               a_dout => ram_instruction_port_data1,

               b_clk => cpuclock,
               b_wr => mc_write(1),
               b_addr => mc_address,
               b_din => mc_write_data,
               b_dout => mc_read_data
               );
  
  ram2: entity work.ram2
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_instruction_port_address,
               a_din => (others => '0'),
               a_dout => ram_instruction_port_data2,

               b_clk => cpuclock,
               b_wr => mc_write(2),
               b_addr => mc_address,
               b_din => mc_write_data,
               b_dout => mc_read_data
               );
  
  ram3: entity work.ram3
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_instruction_port_address,
               a_din => (others => '0'),
               a_dout => ram_instruction_port_data3,

               b_clk => cpuclock,
               b_wr => mc_write(3),
               b_addr => mc_address,
               b_din => mc_write_data,
               b_dout => mc_read_data
               );
  
  instruction_prefetcher: entity work.gs4502b_instruction_prefetch
    port map (
      cpuclock => cpuclock,
      current_cpu_personality => stage_execute_cpu_personality,

      address_redirecting => stage_execute_redirecting,
      redirected_address => stage_execute_redirected_address,
      redirected_pch => stage_execute_redirected_pch,
      stall => decode_stalling,

      instruction_out => stage_prefetch_instruction,

      memory_address => ram_instruction_port_address,
      memory_data0 => ram_instruction_port_data0,
      memory_data1 => ram_instruction_port_data1,
      memory_data2 => ram_instruction_port_data2,
      memory_data3 => ram_instruction_port_data3
      
      );  
  
  to_stop_ghdl_bug: block
  begin
  decode_stage: entity work.gs4502b_stage_decode
    port map (
      cpuclock => cpuclock,
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

      instruction_in => stage_prefetch_instruction,
      instruction_out => stage_decode_instruction,
      
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

      instruction_in => stage_decode_instruction,
      instruction_out => stage_validate_instruction,
      instruction_valid => stage_validate_instruction_valid,      
      instruction_address_is_as_expected => instruction_address_is_as_expected,

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

      reg_map_low => reg_map_low,
      reg_mb_low => reg_mb_low,
      reg_offset_low => reg_offset_low,
      reg_map_high => reg_map_high,
      reg_mb_high => reg_mb_high,
      reg_offset_high => reg_offset_high,
      cpuport_ddr => cpuport_ddr,
      cpuport_value => cpuport_value,

      monitor_pc => monitor_pc,
      
      stall => memory_stalling,
      instruction_in => stage_validate_instruction,
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

end behavioural;
