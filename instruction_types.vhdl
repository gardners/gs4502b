library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.types.all;
use work.types.resource_names;
use work.instruction_equations.all;
use work.addressing_modes.all;
use work.extra_instruction_equations.all;

package instruction_types is

  type instruction_information is record
    -- Does this instruction load and/or store memory?
    -- (both are set for a RMW instruction)

    -- The bytes, CPU personality and address (PC and translated address)
    -- uniquely identify the instruction
    bytes : instruction_bytes;
    cpu_personality : cpu_personality;
    pc : unsigned(15 downto 0);
    translated : translated_address;

    -- Data computed from the opcode
    addressing_mode : addressing_mode;
    instruction_flags : instruction_flags;
    instruction_extra_flags : extra_instruction_flags;

    -- Address of argument
    argument_address : unsigned(15 downto 0);
    argument_translated : translated_address;

    -- Transaction ID for any indirect vector that this instruction requires
    -- (implied by addressing_mode.indirect)
    vector_fetch_transaction : unsigned(4 downto 0);
    
    -- Once we know both the opcode and the arguments, we can work out if the
    -- instruction can modify the memory map. This means looking for MAP
    -- instruction, as well as writes to $0000, $0001 (C64 ROM banking) or $D030
    -- (C65 ROM banking).  We also throw TAB into this, as it changes the
    -- behaviour of ZP addressing mode, and so we also need to flush the
    -- pipeline.
    modifies_cpu_personality : boolean;
    
    -- Address and PC information following instruction    
    pc_expected : unsigned(15 downto 0);
    pc_mispredict : unsigned(15 downto 0);

    expected_translated : translated_address;
    mispredict_translated : translated_address;

    -- Do we expect this branch to be taken?
    branch_predict : boolean;
    
  end record;
  
end package;
