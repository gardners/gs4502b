library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

package icachetypes is
  
  type instruction_data is record
    dummy : std_logic;
      
  end record;
  
  type icache_line is record
    address : unsigned(31 downto 0);
    decode : instruction_data;
    byte2 : unsigned(7 downto 0);
    byte3 : unsigned(7 downto 0);
    next_pc : unsigned(15 downto 0);
    branch_pc : unsigned(15 downto 0);
    alu_op : std_logic_vector(3 downto 0);
    src : std_logic_vector(2 downto 0);
    branch_bits : std_logic_vector(2 downto 0);
    
    padding : std_logic_vector(8 downto 0);
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
    i.branch_pc := unsigned(bits(87 downto 72));

    -- 106 - 88 = 18 instruction decode bits: is it enough?
    -- do we need to merge byte2 and byte3, so that they do double duty with
    -- branch_pc? Probably something like that

    -- Instruction decode flags
    -- ALU OP (ADC,SBC,ASL,LSR,INC,DEC or NOP)
    i.alu_op := std_logic_vector(bits(91 downto 88));
    -- SRC (A,X,Y,Z,FLAGS or M) (for ALU and for PHn)
    i.src := std_logic_vector(bits(94 downto 92));
    -- BRANCH FLAG TO BE SATISFIED (N,Z,C,V or none), and whether instruction
    -- is a branch
    i.branch_bits := std_logic_vector(bits(97 downto 95));
                       
    -- WHICH FLAGS GET SET BY WHICH UNIT (ALU, MEM READ)


    return i;    
    
  end function;
  
end icachetypes;
