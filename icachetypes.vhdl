library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

package icachetypes is
    
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

  type addressing_mode is (
    Implied,
    Immediate8,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    -- For pushing values to the stack (allows PH<X> instructions to be
    -- considered as simple stores, which is what they are).
    StackWrite,
    -- For loading registers, including when pulling something from the stack.
    StackRead,
    -- Merges both Relative8 and Relative16 functions
    ConditionalBranch
    -- XXX - ZP+Relative8 mode not currently considered.
    -- XXX Add the rest
    );

  type instruction is (
    -- Nop here actually covers a few different things, including JMP and BRA,
    -- where all we need to do is unconditionally change the program counter.
    Nop,
    -- One of various typical ALU-focused operations, such as ADC, SBC, ORA,
    -- EOR etc.
    Alu,
    -- Pull a value off the stack or read a memory location (no matter which register is the destination)
    Load,
    -- Store a value, either to the stack or via an addressing mode
    Store
    -- XXX add the rest
    );

  type cpu_personality is (
    Hypervisor, CPU6502, CPU4502
    );
  
  type instruction_information is record
    -- Does this instruction load and/or store memory?
    -- (both are set for a RMW instruction)
    does_load : boolean;
    does_store : boolean;
    modifies_cpu_personality : boolean;
    
    addressing_mode : addressing_mode;
    instruction : instruction;

    cpu_personality : cpu_personality;

    bytes : instruction_bytes;
    
    -- Address and PC information
    translated : translated_address;
    pc : unsigned(15 downto 0);
    
    pc_expected : unsigned(15 downto 0);
    pc_mispredict : unsigned(15 downto 0);

    expected_translated : translated_address;
    mispredict_translated : translated_address;
      
    branch_predict : boolean;
    -- XXX add information for conditional branches
    -- (i.e., which flag and positive or negative test)
    
  end record;

  function to_std_logic_vector(c : cpu_personality) return std_logic_vector;
  function to_cpu_personality(v : std_logic_vector(1 downto 0)) return cpu_personality;

  
end package;

package body icachetypes is

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
