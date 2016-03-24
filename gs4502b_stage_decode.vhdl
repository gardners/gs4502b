-- Cache line provides bottom 10 bits of 32-bit address
-- Cache .address provides bits 31 downto 10 of 32-bit address
-- Cache .pch provides bits 15 downto 8 of PC for this instruction as intended
-- to be run.  This allows the target PC of any branching instruction to be
-- pre-computed in the cache line. Both branching and non-branching PC values
-- can be fed into address translators in this stage

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;

entity gs4502b_stage_decode is
  port (
    cpuclock : in std_logic;
    
-- Input: 32-bit address source of instruction (bottom bits come from cache
--        line ID, to save space and avoid need to initialise cache).
    icache_src_address_in : in unsigned(31 downto 10);
-- Input: 3 instruction bytes
    icache_bytes_in : in unsigned(23 downto 0);
-- Input: 8-bit PCH (PC upper byte) for this instruction
    pch_in : in unsigned(15 downto 8);
-- Input: 16-bit PC for expected case
    pc_expected : in unsigned(15 downto 0);
-- Input: 16-bit PC for branch mis-predict case
    pc_mispredict : in unsigned(15 downto 0);
-- Input: 1-bit Branch prediction flag: 1=assume take branch
    branch_predict_in : in std_logic;
-- Input: 1-bit flag + cache line ID from execute stage to instruct us to
--        divert (whether due to branch mis-predict, RTS/RTI, interrupt or trap
--        entry/return).
    cpu_divert : in std_logic;
    cpu_divert_line : in unsigned(9 downto 0);

-- Output: 32-bit address source of instruction
    icache_src_address_out : out translated_address;
-- Output: 10-bit cache line number, so that we can detect cache misses
    icache_line_number_out : out unsigned(9 downto 0);
-- Output: 3 instruction bytes
    icache_bytes_out : out instruction_bytes;
-- Output: 8-bit PCH (PC upper byte) for this instruction
    pch_out : out unsigned(15 downto 8);
-- Output: Translated PC for expected case
    pc_expected_translated : out translated_address;
-- Output: 16-bit PC for branch mis-predict case
    pc_mispredict_translated : out translated_address;
-- Output: Instruction decode signals that can be computed
-- Output: 1-bit Branch prediction flag: 1=assume take branch
--         (for passing to MMU if branch prediction is wrong, so that cache
--         line can be updated).
    branch_predict_out : out std_logic;
-- Output: Cache line for the following instruction, consisting of lower
--         bits of PC value for conditional branch or not, based on branch
--         prediction flag.
--         UNLESS execute stage tells us we need to change PC abnormally
--         (eg branch mis-prediction, RTS/RTI, interrupt or trap entry/return)
    next_cache_line : out unsigned(9 downto 0);

-- Output: Instruction information for this instruction
    instruction_information : out instruction_information;

    stall : in std_logic;
    
    -- Inputs required for address translators
    reg_mb_low : in unsigned(11 downto 0);
    reg_offset_low : in unsigned(11 downto 0);
    reg_map_low : in std_logic_vector(3 downto 0);
    reg_mb_high : in unsigned(11 downto 0);
    reg_map_high : in std_logic_vector(3 downto 0);
    reg_offset_high : in unsigned(11 downto 0);
    cpuport_value : in std_logic_vector(2 downto 0);
    cpuport_ddr : in std_logic_vector(2 downto 0);
    rom_at_8000 : in std_logic;
    rom_at_a000 : in std_logic;
    rom_at_c000 : in std_logic;
    rom_at_e000 : in std_logic;
    viciii_iomode : in std_logic_vector(1 downto 0)
   
    );
end gs4502b_stage_decode;

architecture behavioural of gs4502b_stage_decode is

  signal icache_line_number : unsigned(9 downto 0);
  signal cache_read_address_1 : unsigned(9 downto 0);
  signal most_recently_requested_cache_line : unsigned(9 downto 0);

begin

  -- Delay line for icache read address, so that we know which address is being
  -- output during any particular cycle
  -- (Used for PCH checking, and also for pipeline stalling)
  process(cpuclock)
  begin
    if (rising_edge(cpuclock)) then
      -- We assume that the instruction cache RAM has a latency of one cycle.
      cache_read_address_1 <= most_recently_requested_cache_line;
      icache_line_number <= cache_read_address_1;
      report "I-CACHE is currently presenting line $" & to_hstring(icache_line_number);

    end if;
  end process;
  
  process(cpuclock)
    variable next_line : unsigned(9 downto 0);
    variable icache_bytes : instruction_bytes;
  begin
    if (rising_edge(cpuclock)) then
      icache_bytes.opcode := icache_bytes_in(7 downto 0);
      icache_bytes.arg1 := icache_bytes_in(15 downto 8);
      icache_bytes.arg2 := icache_bytes_in(23 downto 16);
      
      
      if stall='0' then
        icache_src_address_out(31 downto 10) <= icache_src_address_in;
        icache_src_address_out(9 downto 0) <= icache_line_number;
        icache_bytes_out <= icache_bytes;
        -- XXX Need to give line number as output. This requires a delay register
        -- that always shows the correct value.
        icache_line_number_out <= icache_line_number;
        pch_out <= pch_in;
        branch_predict_out <= branch_predict_in;
        next_line := pc_expected(9 downto 0);

        -- Decode instruction
        -- XXX Read fields from instruction cache
        instruction_information.does_load <= false;
        instruction_information.does_store <= false;
        instruction_information.addressing_mode <= Implied;
        instruction_information.instruction <= Nop;
        
        -- CPU personality is only modified by writing to $D02F or $D640-$D67F
        
        if ((icache_bytes.arg2 = x"D0") and (icache_bytes.arg1 = x"2F"))
          or ((icache_bytes.arg2 = x"D6")
              and (icache_bytes.arg1(7 downto 6) = "01")) then
          instruction_information.modifies_cpu_personality <= true;
        else
          instruction_information.modifies_cpu_personality <= false;
        end if;
        
      else
        -- Pipeline stalled: hold existing values.

        -- XXX: Work out the right address to ask from the instruction cache
        -- so that it gets automatically presented again as soon as possible.
        -- This will require a little delay register that shows the correct value
        -- at any point in time.
        -- (If we don't do this, the pipeline may in fact never resume)
        -- It is easy to make it work in a basic way, by just repeatedly asking
        -- for the cache line that would have just been read in.  That will do
        -- for now, even if it isn't totally ideal.  It does mean that unstalling
        -- the pipeline will happen without latency, just that there will then
        -- be a few cycles delay after that one instruction before the pipline
        -- starts to refill.  That's probably okay.
        next_line := icache_line_number;

        -- XXX Make sure that outputs are invalid for next stage, since the
        -- address translators are running no matter what. Or else allow
        -- address translators to be able to stall.
        
      end if;
      -- Finally, if the CPU is elsewhere asking us to divert somewhere, then
      -- do indeed divert there.
      if cpu_divert = '1' then
        report "CPU requests diversion to an address ending in $"
          & to_hstring(cpu_divert_line);
        next_line := cpu_divert_line;
      end if;

      report "I-CACHE read address set to $" & to_hstring(next_line);
      next_cache_line <= next_line;
      most_recently_requested_cache_line <= next_line;

    end if;    
  end process;    

  address_translator0: entity work.address_translator
    port map (
      cpuclock => cpuclock,
      cpuport_value => cpuport_value,
      cpuport_ddr => cpuport_ddr,
      viciii_iomode => viciii_iomode,
      rom_from_colour_ram => '0',
      reg_map_low => reg_map_low,
      reg_mb_low => reg_mb_low,
      reg_offset_low => reg_offset_low,
      reg_map_high => reg_map_high,
      reg_mb_high => reg_mb_high,
      reg_offset_high => reg_offset_high,
      rom_at_8000 => rom_at_8000,
      rom_at_a000 => rom_at_a000,
      rom_at_c000 => rom_at_c000,
      rom_at_e000 => rom_at_e000,

      address_in => pc_expected,
      read_address => pc_expected_translated
      );
  
  address_translator1: entity work.address_translator
    port map (
      cpuclock => cpuclock,
      cpuport_value => cpuport_value,
      cpuport_ddr => cpuport_ddr,
      viciii_iomode => viciii_iomode,
      rom_from_colour_ram => '0',
      reg_map_low => reg_map_low,
      reg_mb_low => reg_mb_low,
      reg_offset_low => reg_offset_low,
      reg_map_high => reg_map_high,
      reg_mb_high => reg_mb_high,
      reg_offset_high => reg_offset_high,
      rom_at_8000 => rom_at_8000,
      rom_at_a000 => rom_at_a000,
      rom_at_c000 => rom_at_c000,
      rom_at_e000 => rom_at_e000,

      address_in => pc_mispredict,
      read_address => pc_mispredict_translated
      );
    
end behavioural;
