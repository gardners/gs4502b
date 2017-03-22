
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
  function visualise(e : string; s : string; v : std_logic) return boolean;
  function visualise(e : string; s : string; v : std_logic_vector) return boolean;
  function visualise(e : string; s : string; v : unsigned) return boolean;
  function visualise(e : string; s : string; v : fetch_port_in) return boolean;
  
end package;

package body visualise is

  function visualise(e : string; s : string; v : boolean) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":" & boolean'image(v);
    return true;
  end function;
  
  function visualise(e : string; s : string; v : std_logic) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":" & std_logic'image(v);
    return true;
  end function;
  
  function visualise(e : string; s : string; v : std_logic_vector) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":$" & to_hstring(v);
    return true;
  end function;

  function visualise(e : string; s : string; v : unsigned) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":$" & to_hstring(v);
    return true;
  end function;

  function visualise(e : string; s : string; v : fetch_port_in) return boolean is
  begin
    report "VISUALISE:" & e & ":" & s & ":"
      & boolean'image(v.valid) & ","
      & "$" & to_hstring(v.translated) & ","
      & to_string(v.user_flags);
    return true;
  end function;

  
end package body;
