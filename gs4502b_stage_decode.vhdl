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

entity gs4502b_stage_decode
  port (
    cpuclock : in std_logic;
    
-- Input: 32-bit address source of instruction (bottom bits come from cache
--        line ID, to save space and avoid need to initialise cache).
    icache_src_address_in : in unsigned(31 downto 10);
    icache_line_in : in unsigned(9 downto 0);
-- Input: 3 instruction bytes
    icache_bytes_in : in std_logic_vector(23 downto 0);
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
    icache_src_address_out : out unsigned(31 downto 10);
    icache_line_out : out unsigned(9 downto 0);
-- Output: 3 instruction bytes
    icache_bytes_out : out std_logic_vector(23 downto 0);
-- Output: 8-bit PCH (PC upper byte) for this instruction
    pch_out : out unsigned(15 downto 8);
-- Output: Translated PC for expected case
        pc_expected_translated : unsigned(31 downto 0);
-- Output: 16-bit PC for branch mis-predict case
        pc_mispredict_translated : unsigned(31 downto 0);
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
    next_icache_line : out unsigned(9 downto 0);

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

  component address_translator IS
    PORT (
      cpuclock : IN STD_LOGIC;

      -- Things that affect address mapping
      cpuport_value: in std_logic_vector(2 downto 0);
      cpuport_ddr: in std_logic_vector(2 downto 0);
      viciii_iomode : in std_logic_vector(1 downto 0);
      rom_from_colour_ram : in std_logic;
      reg_map_low : in std_logic_vector(3 downto 0);
      reg_mb_low : in unsigned(11 downto 0);
      reg_offset_low : in unsigned(11 downto 0);
      reg_map_high : in std_logic_vector(3 downto 0);
      reg_mb_high : in unsigned(11 downto 0);
      reg_offset_high : in unsigned(11 downto 0);
      rom_at_c000 : in std_logic;
      rom_at_e000 : in std_logic;
      rom_at_a000 : in std_logic;
      rom_at_8000 : in std_logic;

      memory_map_has_changed : out std_logic := '0';
    
      address_in : in unsigned(15 downto 0);
      read_address : out unsigned(31 downto 0);
      write_address : out unsigned(31 downto 0)
      );
  END component;

begin

  process(cpuclock)
  begin
    if (rising_edge(cpuclock)) then
      icache_src_address_out <= icache_src_address_in;
      icache_line_out <= icache_line_in;
      icache_bytes_out <= icache_bytes_in;
      pch_out <= pch_in;
      branch_predict_out <= branch_predict_in;
      if cpu_divert = '1' then
        next_cache_line <= cpu_divert_line;
      else
        next_cache_line <= pc_expected(9 downto 0);
      end if;
      -- XXX - Decode instruction
    end if;    
  end process;    
  
  address_translator0: address_translator
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

      address_in => address_translator0_address_in,
      read_address => address_translator0_address_out
      );
  
  address_translator1: address_translator
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

      address_in => address_translator1_address_in,
      read_address => address_translator1_address_out
      );
    
end behavioural;
