
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

  function visualise(e : string; s : string; v : boolean) return boolean;
  function visualise(e : string; s : string; v : std_ulogic) return boolean;
  function visualise(e : string; s : string; v : std_logic) return boolean;
  function visualise(e : string; s : string; v : std_logic_vector) return boolean;
  function visualise(e : string; s : string; v : unsigned) return boolean;
  function visualise(e : string; s : string; v : fetch_port_in) return boolean;
  function visualise(e : string; s : string; v : fetch_port_out) return boolean;
  function visualise(e : string; s : string; v : mem_port_in) return boolean;
  function visualise(e : string; s : string; v : mem_port_out) return boolean;

end package;

package body visualise is

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

  function visualise(e : string; s : string; v : std_ulogic) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":bit:" & std_ulogic'image(v);
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


  
end package body;
