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
use work.instructions.all;
use work.instruction_equations.all;
use work.address_translator.all;

entity gs4502b_stage_decode is
  port (
    cpuclock : in std_logic;

    current_cpu_personality : in cpu_personality;

    instruction_in : in instruction_information;
    
-- Input: 1-bit flag + cache line ID from execute stage to instruct us to
--        divert (whether due to branch mis-predict, RTS/RTI, interrupt or trap
--        entry/return).
    address_redirecting : in boolean;
    redirected_address : in translated_address;

-- Output: 32-bit address source of instruction
    instruction_out : out instruction_information;

    stall : in boolean;
    stalling : out boolean := false;
    
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

  signal stalled_instruction : instruction_information;
  signal stall_buffer_occupied : boolean := false;
  
begin

  process(cpuclock)
    variable next_line : unsigned(9 downto 0);
    variable instruction : instruction_information;
  begin
    if (rising_edge(cpuclock)) then

      if stall_buffer_occupied then
        instruction := stalled_instruction;
      else
        instruction := instruction_in;
      end if;
      
      if stall = false then
        report "$" & to_hstring(instruction.translated) &
          " DECODE : Not stalled. Decoding. reg_map_high="
          & to_string(reg_map_high)
          & ", reg_mb_high=$" & to_hstring(reg_mb_high);
      
        -- Decode instruction
        -- XXX Read fields from instruction bytes and work it all out
        -- For now, just lie and make every instruction an NOP

        -- Work out branch address of instruction, if relevant
        -- instruction.pc_mispredict := pc_mispredict;
        instruction.expected_translated
          := resolve_address_to_long(instruction.pc_expected,
                                     false,
                                     
                                     cpuport_value,cpuport_ddr,
                                     viciii_iomode,
                                     reg_map_low,
                                     reg_mb_low,
                                     reg_offset_low,
                                     reg_map_high,
                                     reg_mb_high,
                                     reg_offset_high,
                                     rom_at_8000,
                                     rom_at_a000,
                                     rom_at_c000,
                                     rom_at_e000);
        instruction.mispredict_translated
          := resolve_address_to_long(instruction.pc_mispredict,
                                     false,
                               
                                     cpuport_value,cpuport_ddr,
                                     viciii_iomode,
                                     reg_map_low,
                                     reg_mb_low,
                                     reg_offset_low,
                                     reg_map_high,
                                     reg_mb_high,
                                     reg_offset_high,
                                     rom_at_8000,
                                     rom_at_a000,
                                     rom_at_c000,
                                     rom_at_e000);
        
        -- CPU personality is only modified by writing to $D02F or $D640-$D67F
        
        if ((instruction.bytes.arg2 = x"D0") and (instruction.bytes.arg1 = x"2F"))
          or ((instruction.bytes.arg2 = x"D6")
              and (instruction.bytes.arg1(7 downto 6) = "01")) then
          instruction.modifies_cpu_personality := true;
        else
          instruction.modifies_cpu_personality := false;
        end if;

        if stall_buffer_occupied then
          stall_buffer_occupied <= false;
          stalling <= true;
        else
          stalling <= false;
        end if;
        
      else
        -- Pipeline stalled: hold existing values.
        report "$" & to_hstring(instruction.translated) &
          " DECODE : Stalled -- holding values.";
        stall_buffer_occupied <= true;
        stalled_instruction <= instruction;
        stalling <= true;
      end if;
      
      -- Set instruction flags based on CPU personality and opcode
      if instruction.cpu_personality = CPU6502 then
        instruction.instruction_flags
          := get_instruction_flags("0"&std_logic_vector(instruction.bytes.opcode));
      else
        instruction.instruction_flags
          := get_instruction_flags("1"&std_logic_vector(instruction.bytes.opcode));
      end if;

      instruction_out <= instruction;

      report "instruction.aludst_a = " & boolean'image(instruction.instruction_flags.aludst_a) & ", opcode=$" & to_hstring(instruction.bytes.opcode);

    end if;    
  end process;    
    
end behavioural;
