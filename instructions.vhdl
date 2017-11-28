library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use work.types.all;

package instructions is

  
  function "and" (a : instruction_resources; b : instruction_resources)
    return instruction_resources;

  function "=" (a : instruction_resources; b : boolean)
    return instruction_resources;
  
  function not_empty(a : instruction_resources) return boolean;

  function to_instruction_bytes(op : unsigned(7 downto 0); arg1 : unsigned(7 downto 0);
                                arg2 : unsigned(7 downto 0)) return instruction_bytes;
  
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
    r.a := b;
    r.b := b;
    r.x := b;
    r.y := b;
    r.z := b;
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
    r.a := a.a and b.a;
    r.b := a.b and b.b;
    r.x := a.x and b.x;
    r.y := a.y and b.y;
    r.z := a.z and b.z;
    r.flag_c := a.flag_c and b.flag_c;
    r.flag_d := a.flag_d and b.flag_d;
    r.flag_n := a.flag_n and b.flag_n;
    r.flag_v := a.flag_v and b.flag_v;
    r.flag_z := a.flag_z and b.flag_z;
    
    return r;
  end function;

  function not_empty(a : instruction_resources) return boolean is
  begin
    return a.a or a.b or a.x or a.y or a.z
      or a.flag_z or a.flag_n or a.flag_c or a.flag_d or a.flag_v;
  end function;
  
end instructions;
