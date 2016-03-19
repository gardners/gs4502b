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

end package;

package body icachetypes is
  
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
