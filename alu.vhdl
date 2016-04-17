

library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use work.debugtools.all;
use work.instruction_equations.all;
use work.instructions.all;
use work.extra_instruction_equations.all;

package alu is

  subtype instruction_length is integer range 1 to 3;
  type ilens4 is array (0 to 3) of instruction_length;
  type bytes4 is array (0 to 3) of std_logic_vector(8 downto 0);
  
  type fetch_port_in is record
    valid : boolean;
    translated : translated_address;
    user_flags : std_logic_vector(7 downto 0);
  end record;
  type fetch_port_out is record
    -- Announce when we can accept more input.
    ready : boolean;
    -- 4 bytes of read value
    bytes : bytes4;
    -- Address of request
    translated : translated_address;
    -- User specified flags that were presented with the request
    -- when submitted.
    user_flags : std_logic_vector(7 downto 0);
  end record;  
  type mem_port_in is record
    valid : boolean;
  end record;
  type mem_port_out is record
    result : transaction_result;
    -- Tell user if previous value has been accepted
    acknowledged : boolean;
  end record;

  type ram_interface is record
    iaddr : std_logic_vector(16 downto 0);
    maddr : std_logic_vector(16 downto 0);
    mwrite : std_logic;
    mwdata : std_logic_vector(8 downto 0);
  end record;  
  
  type alu_result is record
    value : unsigned(7 downto 0);
    c : boolean;
    n : boolean;
    z : boolean;
    v : boolean;
  end record;

  type cpu_flags is record
    c : boolean;
    d : boolean;
    i : boolean;
    z : boolean;
    e : boolean;
    v : boolean;
    n : boolean;
  end record;
  
  type cpu_registers is record
    flags : cpu_flags;
    a : unsigned(7 downto 0);
    a_dup1 : unsigned(7 downto 0);
    a_dup2 : unsigned(7 downto 0);
    a_dup3 : unsigned(7 downto 0);
    b : unsigned(7 downto 0);
    x : unsigned(7 downto 0);
    y : unsigned(7 downto 0);
    z : unsigned(7 downto 0);
    spl : unsigned(7 downto 0);
    sph : unsigned(7 downto 0);
  end record;
  
  function to_string(flags : in cpu_flags) return string;
  
  function alu_op (
    instruction : in instruction_flags;
    i1 : in unsigned(7 downto 0);
    i2 : in unsigned(7 downto 0);
    c_in : boolean;
    d_in : boolean) return alu_result;

  procedure do_reg_op(regs : in cpu_registers;
                      regsout : out cpu_registers;
                      renamed : in instruction_resources;
                      renamedout : out instruction_resources;
                      iflags : in instruction_flags;
                      extraflags : in extra_instruction_flags;
                      i2 : in unsigned(7 downto 0)
                      );

end package;

package body alu is

  function to_string(flags : in cpu_flags) return string is
    variable flag_string : string(1 to 8);
  begin
    flag_string := "--------";
    if flags.n then flag_string(1):='N'; end if;
    if flags.v then flag_string(2):='V'; end if;
    if flags.e then flag_string(3):='E'; end if;
    if flags.d then flag_string(5):='D'; end if;
    if flags.i then flag_string(6):='I'; end if;
    if flags.z then flag_string(7):='Z'; end if;
    if flags.c then flag_string(8):='C'; end if;
    
    return flag_string;
  end function;
  
  function alu_op_cmp (
    i1 : in unsigned(7 downto 0);
    i2 : in unsigned(7 downto 0)) return alu_result is
    variable result : unsigned(8 downto 0);
    variable ret : alu_result;
  begin
    result := ("0"&i1) - ("0"&i2);
    ret.z := false;
    ret.n := false;
    ret.v := false;
    ret.c := false;
    ret.value := result(7 downto 0);
    if result(7 downto 0)=x"00" then
      ret.z := true;
    end if;
    if result(8)='0' then
      ret.c := true;
    end if;
    if result(7)='1' then
      ret.n := true;
    end if;
    return ret;
  end alu_op_cmp;
  
  function alu_op_add (
    flag_c : in boolean;
    flag_d : in boolean;
    i1 : in unsigned(7 downto 0);
    i2 : in unsigned(7 downto 0)) return alu_result is
    -- Result is NVZC<8bit result>
    variable tmp : unsigned(11 downto 0);
    variable ret : alu_result;
    variable c : std_logic := '0';
    variable v : std_logic := '0';
  begin
    ret.n := false; ret.z := false; ret.c := false; ret.v := false;
    if flag_c then
      c := '1';
    end if;
    if flag_d then
      tmp(8) := '0';
      tmp(7 downto 0) := (i1 and x"0f") + (i2 and x"0f") + ("0000000" & c);
      
      if tmp(7 downto 0) > x"09" then
        tmp(7 downto 0) := tmp(7 downto 0) + x"06";
      end if;
      if tmp(7 downto 0) < x"10" then
        tmp(8 downto 0) := '0'&(tmp(7 downto 0) and x"0f")
                           + to_integer(i1 and x"f0") + to_integer(i2 and x"f0");
      else
        tmp(8 downto 0) := '0'&(tmp(7 downto 0) and x"0f")
                           + to_integer(i1 and x"f0") + to_integer(i2 and x"f0")
                           + 16;
      end if;
      if (i1 + i2 + ( "0000000" & c )) = x"00" then
        report "add result SET Z";
        ret.z := true;
      end if;
      if tmp(7) = '1' then
        ret.n := true;
      end if;
      v := (i1(7) xor tmp(7)) and (not (i1(7) xor i2(7))); -- V flag
      if v = '1' then
        ret.v := true;
      end if;
      if tmp(8 downto 4) > "01001" then
        tmp(7 downto 0) := tmp(7 downto 0) + x"60";
        ret.c := true;
      end if;
    else
      tmp(8 downto 0) := ("0"&i2)
                         + ("0"&i1)
                         + ("00000000"&c);
      tmp(7 downto 0) := tmp(7 downto 0);
      if tmp(7) = '1' then
        ret.n := true;
      end if;
      tmp(11) := tmp(7); -- N flag
      if (tmp(7 downto 0) = x"00") then
        ret.z := true;
      end if;
      v := (not (i1(7) xor i2(7))) and (i1(7) xor tmp(7)); -- V flag
      if v = '1' then
        ret.v := true;
      end if;
    end if;
    ret.value := tmp(7 downto 0);
    
    return ret;
  end function alu_op_add;
  
  function alu_op_sub (
    flag_c : in boolean;
    flag_d : in boolean;
    i1 : in unsigned(7 downto 0);
    i2 : in unsigned(7 downto 0)) return alu_result is
    variable tmp : unsigned(11 downto 0); -- NVZC+8bit result
    variable tmpd : unsigned(8 downto 0);
    variable c : unsigned(8 downto 0) := (others => '0');
    variable ret : alu_result;
  begin
    if flag_c then
      c(0) := '1';
    end if;
    tmp(8 downto 0) := ("0"&i1) - ("0"&i2) - "000000001" + c;
    tmp(8) := not tmp(8); -- Carry flag
    tmp(10) := (i1(7) xor tmp(7)) and (i1(7) xor i2(7)); -- Overflowflag
    tmp(7 downto 0) := tmp(7 downto 0);
    tmp(11) := tmp(7); -- Negative flag
    if tmp(7 downto 0) = x"00" then
      tmp(9) := '1';
    else
      tmp(9) := '0';  -- Zero flag
    end if;
    if flag_d then
      tmpd := (("00000"&i1(3 downto 0)) - ("00000"&i2(3 downto 0)))
              - "000000001" + c;

      if tmpd(4)='1' then
        tmpd(3 downto 0) := tmpd(3 downto 0)-x"6";
        tmpd(8 downto 4) := ("0"&i1(7 downto 4)) - ("0"&i2(7 downto 4)) - "00001";
      else
        tmpd(8 downto 4) := ("0"&i1(7 downto 4)) - ("0"&i2(7 downto 4));
      end if;
      if tmpd(8)='1' then
        tmpd(8 downto 0) := tmpd(8 downto 0) - ("0"&x"60");
      end if;
      tmp(7 downto 0) := tmpd(7 downto 0);
    end if;
    -- Return final value
    --report "subtraction result of "
    --  & "$" & to_hstring(std_logic_vector(i1)) 
    --  & " - "
    --  & "$" & to_hstring(std_logic_vector(i2)) 
    --  & " - 1 + "
    --  & "$" & std_logic'image(flag_c)
    --  & " = " & to_hstring(std_logic_vector(tmp(7 downto 0))) severity note;
    ret.z := false; ret.v := false; ret.n := false; ret.c := false;
    ret.value := tmp(7 downto 0);
    if tmp(11) = '1' then ret.n := true; end if;
    if tmp(10) = '1' then ret.v := true; end if;
    if tmp(9) = '1' then ret.z := true; end if;
    if tmp(8) = '1' then ret.c := true; end if;
    return ret;
  end function alu_op_sub;

  procedure do_reg_op(regs : in cpu_registers;
                      regsout : out cpu_registers;
                      renamed : in instruction_resources;
                      renamedout : out instruction_resources;
                      iflags : in instruction_flags;
                      extraflags : in extra_instruction_flags;
                      i2 : in unsigned(7 downto 0)
                      ) is
  begin    

    regsout := regs;
    renamedout := renamed;

    if iflags.update_nz then
      regsout.flags.n := false;
      regsout.flags.z := false;
      renamedout.flag_z := false;
      renamedout.flag_n := false;
    end if;
    
    -- Do various register operations
    -- XXX Handle renamed registers here, by copying renamed
    -- status as well as value, including rename transaction id.
    -- XXX Also make sure flags get renamed when transferring from a
    -- renamed register.
    -- Or do we just deem it too hard, and require regs and flags to be resolved
    -- for a register operation? It is much simpler, and won't hurt the benefit
    -- that register renaming provides for memory copies.
    if extraflags.tab then regsout.b := regs.a_dup3; end if;
    if extraflags.tax then
      regsout.x := regs.a_dup3;
      renamedout.x := renamed.a;
    end if;
    if extraflags.tay then
      regsout.y := regs.a_dup3;
      renamedout.y := renamed.a;
    end if;
    if extraflags.taz then
      regsout.z := regs.a_dup3;
      renamedout.z := renamed.a;
    end if;
    if extraflags.tba then
      regsout.a := regs.b;
      renamedout.a := false;
    end if;
    if extraflags.txa then
      regsout.a := regs.x;
      renamedout.a := renamed.x;
    end if;
    if extraflags.tya then
      regsout.a := regs.y;
      renamedout.a := renamed.y;
    end if;
    if extraflags.tza then
      regsout.a := regs.z;
      renamedout.a := renamed.z;
    end if;
    if extraflags.txs then regsout.spl := regs.x; end if;
    if extraflags.tys then regsout.sph := regs.y; end if;
    if extraflags.tsx then regsout.x := regs.spl; end if;
    if extraflags.tsy then regsout.y := regs.sph; end if;
    if extraflags.lda then regsout.a := i2; renamedout.a := false; end if;
    if extraflags.ldx then regsout.x := i2; renamedout.x := false; end if;
    if extraflags.ldy then regsout.y := i2; renamedout.y := false; end if;
    if extraflags.ldz then regsout.z := i2; renamedout.z := false; end if;
    
    if extraflags.nega then
      regsout.a := (not regs.a) + 1;
      regsout.a_dup1 := (not regs.a_dup1) + 1;
      regsout.a_dup2 := (not regs.a_dup2) + 1;
      regsout.a_dup3 := (not regs.a_dup3) + 1;
      if regs.a_dup2 = x"FF" then regsout.flags.z := true; end if;
      if (regs.a_dup2 < x"81") and (regs.a_dup2 /= x"00")  then
        regsout.flags.n := true;
      end if;
    end if;
    if extraflags.inca then
      regsout.a := regs.a + 1;
      regsout.a_dup1 := regs.a_dup1 + 1;
      regsout.a_dup2 := regs.a_dup2 + 1;
      regsout.a_dup3 := regs.a_dup3 + 1;
      if regs.a_dup2 = x"FF" then regsout.flags.z := true; end if;
      if (regs.a_dup2 >= x"7F") and (regs.a_dup2 /= x"FF")  then
        regsout.flags.n := true;
      end if;
    end if;
    if extraflags.deca then
      regsout.a := regs.a - 1;
      regsout.a_dup1 := regs.a_dup1 - 1;
      regsout.a_dup2 := regs.a_dup2 - 1;
      regsout.a_dup3 := regs.a_dup3 - 1;
      if regs.a_dup2 = x"01" then regsout.flags.z := true; end if;
      if (regs.a_dup2 > x"80") or (regs.a_dup2 = x"00")  then
        regsout.flags.n := true;
      end if;
    end if;
    if extraflags.incx then
      regsout.x := regs.x + 1;
      if regs.x = x"FF" then regsout.flags.z := true; end if;
      if (regs.x >= x"7F") and (regs.x /= x"FF")  then
        regsout.flags.n := true;
      end if;
    end if;
    if extraflags.decx then
      regsout.x := regs.x - 1;
      if regs.x = x"01" then regsout.flags.z := true; end if;
      if (regs.x > x"80") or (regs.x = x"00")  then
        regsout.flags.n := true;
      end if;
    end if;
    if extraflags.incy then
      regsout.y := regs.y + 1;
      if regs.y = x"FF" then regsout.flags.z := true; end if;
      if (regs.y >= x"7F") and (regs.y /= x"FF")  then
        regsout.flags.n := true;
      end if;
    end if;
    if extraflags.decy then
      regsout.y := regs.y - 1;
      if regs.y = x"01" then regsout.flags.z := true; end if;
      if (regs.y > x"80") or (regs.y = x"00")  then
        regsout.flags.n := true;
      end if;
    end if;
    if extraflags.incz then
      regsout.z := regs.z + 1;
      if regs.z = x"FF" then regsout.flags.z := true; end if;
      if (regs.z >= x"7F") and (regs.z /= x"FF")  then
        regsout.flags.n := true;
      end if;
    end if;
    if extraflags.decz then
      regsout.z := regs.z - 1;
      if regs.z = x"01" then regsout.flags.z := true; end if;
      if (regs.z > x"80") or (regs.z = x"00")  then
        regsout.flags.n := true;
      end if;
    end if;

    if extraflags.nz_from_i2 then
      if i2(7) = '1' then regsout.flags.n := true; end if;
      if i2 = x"00" then regsout.flags.z := true; end if;
    end if;
    if extraflags.nz_from_a then
      if regs.a_dup2(7) = '1' then regsout.flags.n := true; end if;
      if regs.a_dup2 = x"00" then regsout.flags.z := true; end if;
    end if;
    if extraflags.nz_from_b then
      if regs.b(7) = '1' then regsout.flags.n := true; end if;
      if regs.b = x"00" then regsout.flags.z := true; end if;
    end if;
    if extraflags.nz_from_x then
      if regs.x(7) = '1' then regsout.flags.n := true; end if;
      if regs.x = x"00" then regsout.flags.z := true; end if;
    end if;
    if extraflags.nz_from_y then
      if regs.y(7) = '1' then regsout.flags.n := true; end if;
      if regs.y = x"00" then regsout.flags.z := true; end if;
    end if;
    if extraflags.nz_from_z then
      if regs.z(7) = '1' then regsout.flags.n := true; end if;
      if regs.z = x"00" then regsout.flags.z := true; end if;
    end if;
    if extraflags.nz_from_sph then
      if regs.sph(7) = '1' then regsout.flags.n := true; end if;
      if regs.sph = x"00" then regsout.flags.z := true; end if;
    end if;
    if extraflags.nz_from_spl then
      if regs.spl(7) = '1' then regsout.flags.n := true; end if;
      if regs.spl = x"00" then regsout.flags.z := true; end if;
    end if;
            
  end procedure;

  
  function alu_op (
    instruction : in instruction_flags;
    i1 : in unsigned(7 downto 0);
    i2 : in unsigned(7 downto 0);
    c_in : boolean;
    d_in : boolean) return alu_result is
    variable r : alu_result;
  begin
                                        -- default action is nop, ie output input 1
    r.c := false; r.v := false; r.value := i1;
    if i1(7) = '1' then r.n := true; else r.n := false; end if;
    if i1 = x"00" then  r.z := true; else r.z := false; end if;

    if instruction.alu_set then
                                        -- Use i2 instead of i1
      r.c := false; r.v := false; r.value := i2;
      if i2(7) = '1' then r.n := true; else r.n := false; end if;
      if i2 = x"00" then  r.z := true; else r.z := false; end if;
    end if;

    if instruction.alu_adc then r:= alu_op_add(c_in, d_in, i1, i2); end if;
    if instruction.alu_sbc then r:= alu_op_sub(c_in, d_in, i1, i2); end if;
    if instruction.alu_cmp then r:= alu_op_cmp(i1, i2); end if;
    if instruction.alu_or then
      r.value := unsigned(std_logic_vector(i1) or std_logic_vector(i2));
      r.n := false; r.z := false;
      if r.value(7) = '1' then r.n := true; end if;
      if r.value = "000000000" then r.z := true; end if;
    end if;
    if instruction.alu_and then
      r.value := unsigned(std_logic_vector(i1) and std_logic_vector(i2));
      r.n := false; r.z := false;
      if r.value(7) = '1' then r.n := true; end if;
      if r.value = "000000000" then r.z := true; end if;
    end if;
    if instruction.alu_eor then
      r.value := unsigned(std_logic_vector(i1) xor std_logic_vector(i2));
      r.n := false; r.z := false;
      if r.value(7) = '1' then r.n := true; end if;
      if r.value = "000000000" then r.z := true; end if;
    end if;
    
                                        -- XXX Implement missing ALU operations

    return r;
  end function;

  
end package body;

