
library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use work.debugtools.all;
use work.instruction_equations.all;
use work.instructions.all;
use work.extra_instruction_equations.all;
use work.alu.all;

package visualise is

  function visualise(e : string; s : string; v : string) return boolean;
  function visualise(e : string; s : string; v : boolean) return boolean;
  function visualise(e : string; s : string; v : std_logic) return boolean;
  function visualise(e : string; s : string; v : std_logic_vector) return boolean;
  function visualise(e : string; s : string; v : unsigned) return boolean;
  function visualise(e : string; s : string; v : fetch_port_in) return boolean;
  function visualise(e : string; s : string; v : fetch_port_out) return boolean;
  function visualise(e : string; s : string; v : mem_port_in) return boolean;
  function visualise(e : string; s : string; v : mem_port_out) return boolean;
  function visualise(e : string; s : string; v : instruction_information) return boolean;
  function visualise(e : string; s : string; v : instruction_resources) return boolean;
  function visualise(e : string; s : string; v : bytes4) return boolean;
  function visualise(e : string; s : string; v : extra_instruction_flags) return boolean;
  function visualise(e : string; s : string; v : transaction_id) return boolean;
  function visualise(e : string; s : string; v : cpu_personality) return boolean;
  function visualise(e : string; s : string; v : cpu_registers) return boolean;
  function visualise(e : string; s : string; v : transaction_result) return boolean;
  
end package;

package body visualise is

  function visualise(e : string; s : string; v : string) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":string:" & v;
    return true;
  end function;
  
  function visualise(e : string; s : string; v : boolean) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":boolean:" & boolean'image(v);
    return true;
  end function;
  
  function visualise(e : string; s : string; v : std_logic) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":bit:" & std_logic'image(v);
    return true;
  end function;

  function visualise(e : string; s : string; v : std_logic_vector) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":hex:$" & to_hstring(v);
    return true;
  end function;

  function visualise(e : string; s : string; v : unsigned) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":hex:$" & to_hstring(v);
    return true;
  end function;

  function visualise(e : string; s : string; v : fetch_port_in) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":fetch_port_in:"
      & boolean'image(v.valid) & ","
      & "$" & to_hstring(v.translated) & ","
      & to_string(v.user_flags);
    return true;
  end function;

  function visualise(e : string; s : string; v : fetch_port_out) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":fetch_port_out:"
      & boolean'image(v.ready) & ","
      & "$" & to_hstring(v.translated) & ","
      & "$" & to_hstring(v.bytes(0)) & ","
      & "$" & to_hstring(v.bytes(1)) & ","
      & "$" & to_hstring(v.bytes(2)) & ","
      & "$" & to_hstring(v.bytes(3)) & ","
      & to_string(v.user_flags);
    return true;
  end function;

  function visualise(e : string; s : string; v : mem_port_in) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":mem_port_in:"
      & boolean'image(v.valid);
    return true;
  end function;

  function visualise(e : string; s : string; v : mem_port_out) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":mem_port_out:"
      & boolean'image(v.result.valid) & ","
      & integer'image(v.result.id) & ","
      & "$" & to_hstring(v.result.value) & ","
      & boolean'image(v.result.z) & ","
      & boolean'image(v.result.v) & ","
      & boolean'image(v.result.c) & ","
      & boolean'image(v.result.n) & ","
      & boolean'image(v.acknowledged);
    return true;
  end function;

  function selector(cond : boolean; yes : string; no : string) return string is
  begin
    if (cond) then
      return yes;
    else
      return no;
    end if;   
  end function;
  
  function visualise(e : string; s : string; v : instruction_resources) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":instruction_resources:"
      & selector(v.a,"A","-")
      & selector(v.x,"X","-")
      & selector(v.y,"Y","-")
      & selector(v.z,"Z","-")
      & " "
      & selector(v.sph,"SPH","---")
      & " "
      & selector(v.spl,"SPL","---")
      & " "
      & selector(v.flag_z,"z","-")
      & selector(v.flag_n,"n","-")
      & selector(v.flag_d,"d","-")
      & selector(v.flag_v,"v","-")
      & selector(v.flag_c,"c","-");
    return true;
  end function;

  function visualise(e : string; s : string; v : transaction_result) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":transaction_result:"
      & selector(v.valid,"valid:","INVALID:")
      & "id=" &integer'image(v.id)
      & "value=$" & to_hstring(v.value)
      & "flags="
      & selector(v.z,"z","-")
      & selector(v.v,"v","-")
      & selector(v.c,"c","-")
      & selector(v.n,"n","-");
    return true;
  end function;

  
  function visualise(e : string; s : string; v : bytes4) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":bytes4:"
      & "$" & to_hstring(v(0))
      & " $" & to_hstring(v(1))
      & " $" & to_hstring(v(2))
      & " $" & to_hstring(v(3));
    return true;
  end function;
  
  function visualise(e : string; s : string; v : instruction_information) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":instruction_information:"
      & "[$" & to_hstring(v.bytes.opcode) & " : $" & to_hstring(v.bytes.arg1)
      & " $ " & to_hstring(v.bytes.arg2) & "] "
      & cpu_personality'image(v.cpu_personality)
      & " @ $" & to_hstring(v.pc)
      & " ( $" & to_hstring(v.translated) & ")"
      & " etc...";
    return true;
  end function;

  function visualise(e : string; s : string; v : cpu_personality) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":cpu_personality:"
      & cpu_personality'image(v);
    return true;
  end function;
  
  function visualise(e : string; s : string; v : extra_instruction_flags) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":extra_instruction_flags:"
      & "*extra_instruction_flags*";
    return true;
  end function;

  function visualise(e : string; s : string; v : cpu_registers) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":cpu_registers:"
      & "*cpu_registers*";
    return true;
  end function;
  
  function visualise(e : string; s : string; v : transaction_id) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":transaction_id:"
      & integer'image(v);
    return true;
  end function;

  
end package body;
