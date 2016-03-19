library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

package icachetypes is
  
  type instruction_data is record
    dummy : std_logic;      
  end record;
  
  type icache_line is record
    src_address : unsigned(31 downto 10);
    bytes : std_logic_vector(23 downto 0);
    pch : unsigned(15 downto 8);
    expected_pc : unsigned(15 downto 0);
    branch_mispredict_pc : unsigned(15 downto 0);
    branch_predict : std_logic;
  end record;

  function std_logic_vector_to_icache_line(bits : in std_logic_vector(105 downto 0))
  return icache_line;
  
  type instruction_resources is record
    reg_a : boolean;
    reg_b : boolean;
    reg_x : boolean;
    reg_y : boolean;
    reg_z : boolean;
    reg_spl : boolean;
    reg_sph : boolean;
    flag_z : boolean;
    flag_n : boolean;
    flag_c : boolean;
    flag_d : boolean;
    flag_v : boolean;
  end record;
  
  function "and" (a : instruction_resources; b : instruction_resources)
    return instruction_resources;

  function not_empty(a : instruction_resources) return boolean;
  
end package;

package body icachetypes is

  function "and" (a : instruction_resources; b : instruction_resources)
    return instruction_resources is
    variable r : instruction_resources;
  begin
    r.reg_a := a.reg_a and b.reg_a;
    r.reg_b := a.reg_b and b.reg_b;
    r.reg_x := a.reg_x and b.reg_x;
    r.reg_y := a.reg_y and b.reg_y;
    r.reg_z := a.reg_z and b.reg_z;
    r.reg_spl := a.reg_spl and b.reg_spl;
    r.reg_sph := a.reg_sph and b.reg_sph;
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
  
  function std_logic_vector_to_icache_line(bits : in std_logic_vector(105 downto 0))
  return icache_line is
    variable i : icache_line;
  begin
    -- The address that this cache line is for.
    -- We could imply the cache line from the address read, but then we have to
    -- know the latency of the cache. We can make this optimisation later, but
    -- for now, we will just keep it simple.
    i.src_address(31 downto 10) := unsigned(bits(21 downto 0));
    i.bytes := bits(45 downto 22);
    i.expected_pc := unsigned(bits(61 downto 46));
    i.branch_mispredict_pc := unsigned(bits(77 downto 62));
    i.pch := unsigned(bits(85 downto 78));
    i.branch_predict := bits(86);

    return i;    
  end function;

end icachetypes;
