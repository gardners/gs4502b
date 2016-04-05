

library ieee;
use Std.TextIO.all;
use ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

package alu is

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
    b : unsigned(7 downto 0);
    x : unsigned(7 downto 0);
    y : unsigned(7 downto 0);
    z : unsigned(7 downto 0);
    spl : unsigned(7 downto 0);
    sph : unsigned(7 downto 0);
  end record;
  
  function alu_op (
    instruction : in instruction_flags;
    i1 : in unsigned(7 downto 0);
    i2 : in unsigned(7 downto 0);
    cpuflags : in cpu_flags) return alu_result;

  function do_alu_op(regs : inout cpu_registers;
                     iflags : in instruction_flags;
                     i2 : in unsigned(7 downto 0)
                     ) return alu_result;
  
end package;

package body alu is

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
      ret.z <= true;
    end if;
    if result(8)='0' then
      ret.c <= true;
    end if;
    if result(7)='1' then
      ret.n <= true;
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

  function do_alu_op(regs : inout cpu_registers;
                     iflags : in instruction_flags;
                     i2 : in unsigned(7 downto 0)
                     ) return alu_result is
    variable ret: alu_result;
    variable r : alu_result;
    variable i1: unsigned(7 downto 0) := (others => '1');
  begin    
    ret.value := (others => '1');
    
    if flags.alusrc_a then i1 := regs.a; end if;
    if flags.alusrc_b then i1 := regs.b; end if;
    -- SAX and friends AND A and X to form the input argument to the ALU.
    -- Since we apply the value of A above, and set all inputs to high by
    -- default, we can just AND X with the current value of i1 here.
    if flags.alusrc_x then i1 := (i1 and regs.x); end if;
    if flags.alusrc_y then i1 := regs.y; end if;
    if flags.alusrc_z then i1 := regs.z; end if;
    if flags.alusrc_b then i1 := regs.b; end if;
    if flags.alusrc_p then
      -- For PHP
      i1 := (others => '0');
      if regs.flags.c then i1(0) := '1'; end if;
      if regs.flags.c then i1(0) := '1'; end if;
      if regs.flags.c then i1(0) := '1'; end if;
      if regs.flags.c then i1(0) := '1'; end if;
      if regs.flags.c then i1(0) := '1'; end if;
      if regs.flags.c then i1(0) := '1'; end if;
      if regs.flags.c then i1(0) := '1'; end if;
    end if;
    if flags.alusrc_spl then i1 := regs.spl; end if;
    if flags.alusrc_sph then i1 := regs.sph; end if;

    r := alu_op(i1,i2,reg.flags);

    if flags.aludst_a then regs.a <= ret.value; end if;
    if flags.aludst_b then regs.b <= ret.value; end if;
    if flags.aludst_p then
      -- Set flags from byte: for PLP
      regs.flags <= (others => false);
      -- PLP doesn't change E flag
      regs.flags.e <= regs.flags.e;
      if ret.value(0)='1' then regs.flags.c <= true; end if;
      if ret.value(1)='1' then regs.flags.z <= true; end if;
      if ret.value(2)='1' then regs.flags.i <= true; end if;
      if ret.value(3)='1' then regs.flags.d <= true; end if;
      if ret.value(6)='1' then regs.flags.v <= true; end if;
      if ret.value(7)='1' then regs.flags.n <= true; end if;
    end if;
    if flags.aludst_sph then regs.sph <= ret.value; end if;
    if flags.aludst_spl then regs.spl <= ret.value; end if;
    if flags.aludst_x then regs.x <= ret.value; end if;
    if flags.aludst_y then regs.y <= ret.value; end if;
    if flags.aludst_z then regs.z <= ret.value; end if;
    
  end function;

  
  function alu_op (
    instruction : in instruction_flags;
    i1 : in unsigned(7 downto 0);
    i2 : in unsigned(7 downto 0);
    cpuflags : in cpu_flags) return alu_result is
  begin
    if instructions.alu_nop then
      r.c := false; r.v := false; r.value := i1;
      if i1(7) = '1' then r.n := true; else r.n := false; end if;
      if i1 = x"00" then  r.z := true; else r.z := false; end if;
    end if;
    if instructions.alu_adc then r:= alu_op_adc(cpuflags.c, cpuflags.d, i1, i2); end if;
    if instructions.alu_sbc then r:= alu_op_sub(cpuflags.c, cpuflags.d, i1, i2); end if;
    -- XXX Implement missing ALU operations
    
  end function;

  
end package body;

