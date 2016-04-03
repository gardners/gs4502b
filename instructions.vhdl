library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use work.addressing_modes.all;
use work.instruction_equations.all;

package instructions is
      
  type instruction_resources is record
    reg_a : boolean;
    reg_b : boolean;
    reg_x : boolean;
    reg_y : boolean;
    reg_z : boolean;
    flag_z : boolean;
    flag_n : boolean;
    flag_c : boolean;
    flag_d : boolean;
    flag_v : boolean;
  end record;
  
  function "and" (a : instruction_resources; b : instruction_resources)
    return instruction_resources;

  function "=" (a : instruction_resources; b : boolean)
    return instruction_resources;
  
  function not_empty(a : instruction_resources) return boolean;

  type instruction_bytes is record
    opcode : unsigned(7 downto 0);
    arg1 : unsigned(7 downto 0);
    arg2 : unsigned(7 downto 0);
  end record; 
  
  function to_instruction_bytes(op : unsigned(7 downto 0); arg1 : unsigned(7 downto 0);
                                arg2 : unsigned(7 downto 0)) return instruction_bytes;
  
  -- Allow upto 7 memory transactions in flight at a time
  subtype transaction_id is integer range 0 to 7;

  subtype translated_address is unsigned(31 downto 0);

  
  type transaction_result is record
    valid : boolean;
    id : transaction_id;
    value : unsigned(7 downto 0);
    z : boolean;
    v : boolean;
    c : boolean;
    n : boolean;    
  end record;  

  type cpu_personality is (
    Hypervisor, CPU6502, CPU4502
    );
  
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

    -- Once we know both the opcode and the arguments, we can work out if the
    -- instruction can modify the memory map. This means looking for MAP
    -- instruction, as well as writes to $0000, $0001 (C64 ROM banking) or $D030
    -- (C65 ROM banking)
    modifies_cpu_personality : boolean;
    
    -- Address and PC information following instruction    
    pc_expected : unsigned(15 downto 0);
    pc_mispredict : unsigned(15 downto 0);

    expected_translated : translated_address;
    mispredict_translated : translated_address;

    -- Do we expect this branch to be taken?
    branch_predict : boolean;
    
  end record;

  function to_std_logic_vector(c : cpu_personality) return std_logic_vector;
  function to_cpu_personality(v : std_logic_vector(1 downto 0)) return cpu_personality;

  
end package;

package body instructions is

  function to_cpu_personality(v : std_logic_vector(1 downto 0)) return cpu_personality is
  begin
    case v is
      when "00" => return CPU6502;
      when "01" => return CPU4502;
      when "11" => return Hypervisor;
      when others => return Hypervisor;
    end case;    
  end function;  
  
  function to_std_logic_vector(c : cpu_personality) return std_logic_vector is
  begin
    case c is
      when Hypervisor => return "11";
      when CPU6502 => return "00";
      when CPU4502 => return "01";
    end case;    
  end function;
  
  function to_instruction_bytes(op : unsigned(7 downto 0); arg1 : unsigned(7 downto 0);
                                arg2 : unsigned(7 downto 0)) return instruction_bytes is
    variable i : instruction_bytes;
  begin
    i.opcode := op;
    i.arg1   := arg1;
    i.arg2   := arg2;
    return i;
  end function;

  
  function "=" (a : instruction_resources; b : boolean)
    return instruction_resources is
    variable r : instruction_resources;
  begin
    r.reg_a := b;
    r.reg_b := b;
    r.reg_x := b;
    r.reg_y := b;
    r.reg_z := b;
    r.flag_c := b;
    r.flag_d := b;
    r.flag_n := b;
    r.flag_v := b;
    r.flag_z := b;
    return r;
  end function;
  
  
  function "and" (a : instruction_resources; b : instruction_resources)
    return instruction_resources is
    variable r : instruction_resources;
  begin
    r.reg_a := a.reg_a and b.reg_a;
    r.reg_b := a.reg_b and b.reg_b;
    r.reg_x := a.reg_x and b.reg_x;
    r.reg_y := a.reg_y and b.reg_y;
    r.reg_z := a.reg_z and b.reg_z;
    r.flag_c := a.flag_c and b.flag_c;
    r.flag_d := a.flag_d and b.flag_d;
    r.flag_n := a.flag_n and b.flag_n;
    r.flag_v := a.flag_v and b.flag_v;
    r.flag_z := a.flag_z and b.flag_z;
    
    return r;
  end function;

  function not_empty(a : instruction_resources) return boolean is
  begin
    return a.reg_a or a.reg_b or a.reg_x or a.reg_y or a.reg_z
      or a.flag_z or a.flag_n or a.flag_c or a.flag_d or a.flag_v;
  end function;
  
end instructions;
