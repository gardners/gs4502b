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

  function opcode_will_stall_4502(op : unsigned(7 downto 0)) return boolean;
  
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

  -- Return if an instruction will surely stall in the execute stage.
  -- 
  function opcode_will_stall_4502(op : unsigned(7 downto 0)) return boolean is
  begin
    case op is
      -- Basically any instruction that has a path from memory (other than the
      -- instruction operands) into the ALU or PC.  Everything else clears the
      -- execute stage in a single cycle, by passing hard memory work to the
      -- memory controller (including execution of RMW instructions and stack
      -- push/pops for flags and registers), and simply asserting that some CPU
      -- resources will be locked until those operations complete.
      when x"65" => -- ADC $nn             65   6502
      when x"75" => -- ADC $nn,X           75   6502
      when x"6D" => -- ADC $nnnn           6D   6502
      when x"7D" => -- ADC $nnnn,X         7D   6502
      when x"79" => -- ADC $nnnn,Y         79   6502
      when x"71" => -- ADC ($nn),Y         71   6502
      when x"72" => -- ADC ($nn),Z         72   65C02/65CE02
      when x"61" => -- ADC ($nn,X)         61   6502
      when x"25" => -- AND $nn             25   6502
      when x"35" => -- AND $nn,X           35   6502
      when x"2D" => -- AND $nnnn           2D   6502
      when x"3D" => -- AND $nnnn,X         3D   6502
      when x"39" => -- AND $nnnn,Y         39   6502
      when x"31" => -- AND ($nn),Y         31   6502
      when x"32" => -- AND ($nn),Z         32   65C02/65CE02
      when x"21" => -- AND ($nn,X)         21   6502
      when x"0F" => -- BBR0 $zp,$nn        0F   65SC02
      when x"1F" => -- BBR1 $zp,$nn        1F   65SC02
      when x"2F" => -- BBR2 $zp,$nn        2F   65SC02
      when x"3F" => -- BBR3 $zp,$nn        3F   65SC02
      when x"4F" => -- BBR4 $zp,$nn        4F   65SC02
      when x"5F" => -- BBR5 $zp,$nn        5F   65SC02
      when x"6F" => -- BBR6 $zp,$nn        6F   65SC02
      when x"7F" => -- BBR7 $zp,$nn        7F   65SC02
      when x"8F" => -- BBS0 $nn,$nn        8F   65SC02
      when x"9F" => -- BBS1 $nn,$nn        9F   65SC02
      when x"AF" => -- BBS2 $nn,$nn        AF   65SC02
      when x"BF" => -- BBS3 $nn,$nn        BF   65SC02
      when x"CF" => -- BBS4 $nn,$nn        CF   65SC02
      when x"DF" => -- BBS5 $nn,$nn        DF   65SC02
      when x"EF" => -- BBS6 $nn,$nn        EF   65SC02
      when x"FF" => -- BBS7 $nn,$nn        FF   65SC02
      when x"24" => -- BIT $nn             24   6502
      when x"34" => -- BIT $nn,X           34   65SC02
      when x"2C" => -- BIT $nnnn           2C   6502
      when x"3C" => -- BIT $nnnn,X         3C   65SC02
      when x"00" => -- BRK                 00   6502
      when x"C5" => -- CMP $nn             C5   6502
      when x"D5" => -- CMP $nn,X           D5   6502
      when x"CD" => -- CMP $nnnn           CD   6502
      when x"DD" => -- CMP $nnnn,X         DD   6502
      when x"D9" => -- CMP $nnnn,Y         D9   6502
      when x"D1" => -- CMP ($nn),Y         D1   6502
      when x"D2" => -- CMP ($nn),Z         D2   65C02/65CE02
      when x"C1" => -- CMP ($nn,X)         C1   6502
      when x"E4" => -- CPX $nn             E4   6502
      when x"EC" => -- CPX $nnnn           EC   6502
      when x"C4" => -- CPY $nn             C4   6502
      when x"CC" => -- CPY $nnnn           CC   6502
      when x"D4" => -- CPZ $nn             D4   65CE02
      when x"DC" => -- CPZ $nnnn           DC   65CE02
      when x"45" => -- EOR $nn             45   6502
      when x"55" => -- EOR $nn,X           55   6502
      when x"4D" => -- EOR $nnnn           4D   6502
      when x"5D" => -- EOR $nnnn,X         5D   6502
      when x"59" => -- EOR $nnnn,Y         59   6502
      when x"51" => -- EOR ($nn),Y         51   6502
      when x"52" => -- EOR ($nn),Z         52   65C02/65CE02
      when x"41" => -- EOR ($nn,X)         41   6502
      when x"6C" => -- JMP ($nnnn)         6C   6502
      when x"7C" => -- JMP ($nnnn,X)       7C   65C02
      when x"22" => -- JSR ($nnnn)         22   65CE02
      when x"23" => -- JSR ($nnnn,X)       23   65CE02
      when x"05" => -- ORA $nn             05   6502
      when x"15" => -- ORA $nn,X           15   6502
      when x"0D" => -- ORA $nnnn           0D   6502
      when x"1D" => -- ORA $nnnn,X         1D   6502
      when x"19" => -- ORA $nnnn,Y         19   6502
      when x"11" => -- ORA ($nn),Y         11   6502
      when x"12" => -- ORA ($nn),Z         12   65C02/65CE02
      when x"01" => -- ORA ($nn,X)         01   6502
      when x"F4" => -- PHW #$nnnn          F4   65CE02
      when x"FC" => -- PHW $nnnn           FC   65CE02
      when x"40" => -- RTI                 40   6502
      when x"60" => -- RTS                 60   6502
      when x"E5" => -- SBC $nn             E5   6502
      when x"F5" => -- SBC $nn,X           F5   6502
      when x"ED" => -- SBC $nnnn           ED   6502
      when x"FD" => -- SBC $nnnn,X         FD   6502
      when x"F9" => -- SBC $nnnn,Y         F9   6502
      when x"F1" => -- SBC ($nn),Y         F1   6502
      when x"F2" => -- SBC ($nn),Z         F2   65C02/65CE02
      when x"E1" => -- SBC ($nn,X)         E1   6502
      when x"14" => -- TRB $nn             14   65SC02
      when x"1C" => -- TRB $nnnn           1C   65SC02
      when x"04" => -- TSB $nn             04   65SC02
      when x"0C" => -- TSB $nnnn           0C   65SC02
      when others =>
        return false;
    end case;
    return true;
  end function;
  
end icachetypes;
