library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

package icachetypes is
  
  type instruction_data is record
    dummy : std_logic;
      
  end record;
  
  type icache_line is record
    address : unsigned(27 downto 10);
    decode : instruction_data;
    byte2 : unsigned(7 downto 0);
    byte3 : unsigned(7 downto 0);
    next_pc : unsigned(15 downto 0);
    
    padding : std_logic_vector((106-28+10
                                -1 -- instruction data
                                -8-8-16) downto 1);
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
    i.address := unsigned(bits(31 downto 0));
    -- The 2nd and 3rd argument bytes of the instruction.
    -- For branch instructions, these actually contain the target PC value, if
    -- the branch is taken.
    i.byte2 := unsigned(bits(47 downto 40));
    i.byte3 := unsigned(bits(55 downto 48));
    -- PC value after executing instruction.
    -- While it wastes a few bits here, it means that the CPU doesn't have to
    -- do any arithmatic on the PC, which should help keep the logic depth as
    -- shallow as possible.
    i.next_pc := unsigned(bits(71 downto 56));

    -- Instruction decode flags
    -- ALU OP (ADC,SBC,ASL,LSR,INC,DEC or NOP)
    -- SRC (A,X,Y,Z,FLAGS or M) (for ALU and for PHn)
    -- BRANCH FLAGS TO BE SATISFIED (N,Z,C,V or none)
    -- IS BRANCH INSTRUCTION
    -- WHICH FLAGS GET SET BY WHICH UNIT (ALU, MEM READ)

    return i;    
    
  end function;
  
end icachetypes;
