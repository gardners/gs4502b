use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.instructions.all;
use work.alu.all;
use work.extra_instruction_equations.all;
use work.addressing_modes.all;

package disassemble is

  type instruction is (
    -- 4510 opcodes
    I_ADC,I_AND,I_ASL,I_ASR,I_ASW,I_BBR,I_BBS,I_BCC,
    I_BCS,I_BEQ,I_BIT,I_BMI,I_BNE,I_BPL,I_BRA,I_BRK,
    I_BSR,I_BVC,I_BVS,I_CLC,I_CLD,I_CLE,I_CLI,I_CLV,
    I_CMP,I_CPX,I_CPY,I_CPZ,I_DEC,I_DEW,I_DEX,I_DEY,    
    I_DEZ,I_EOM,I_EOR,I_INC,I_INW,I_INX,I_INY,I_INZ,    
    I_JMP,I_JSR,I_LDA,I_LDX,I_LDY,I_LDZ,I_LSR,I_MAP,
    I_NEG,I_ORA,I_PHA,I_PHP,I_PHW,I_PHX,I_PHY,I_PHZ,
    I_PLA,I_PLP,I_PLX,I_PLY,I_PLZ,I_RMB,I_ROL,I_ROR,
    I_ROW,I_RTI,I_RTS,I_SBC,I_SEC,I_SED,I_SEE,I_SEI,    
    I_SMB,I_STA,I_STX,I_STY,I_STZ,I_TAB,I_TAX,I_TAY,
    I_TAZ,I_TBA,I_TRB,I_TSB,I_TSX,I_TSY,I_TXA,I_TXS,
    I_TYA,I_TYS,I_TZA,

    -- 6502 illegals
    I_SLO,I_RLA,I_SRE,I_RRA,I_SAX,I_LAX,I_DCP,I_ISC,
    I_ANC,I_ALR,I_ARR,I_XAA,I_AXS,I_AHX,I_SHY,I_SHX,
    I_TAS,I_LAS,I_NOP,I_KIL
    
    );
    type ilut9bit is array(0 to 511) of instruction;

  constant instruction_lut : ilut9bit := (
  -- 4502 personality
    I_BRK,I_ORA,I_CLE,I_SEE,I_TSB,I_ORA,I_ASL,I_RMB,I_PHP,I_ORA,I_ASL,I_TSY,I_TSB,I_ORA,I_ASL,I_BBR,
    I_BPL,I_ORA,I_ORA,I_BPL,I_TRB,I_ORA,I_ASL,I_RMB,I_CLC,I_ORA,I_INC,I_INZ,I_TRB,I_ORA,I_ASL,I_BBR,
    I_JSR,I_AND,I_JSR,I_JSR,I_BIT,I_AND,I_ROL,I_RMB,I_PLP,I_AND,I_ROL,I_TYS,I_BIT,I_AND,I_ROL,I_BBR,
    I_BMI,I_AND,I_AND,I_BMI,I_BIT,I_AND,I_ROL,I_RMB,I_SEC,I_AND,I_DEC,I_DEZ,I_BIT,I_AND,I_ROL,I_BBR,
    I_RTI,I_EOR,I_NEG,I_ASR,I_ASR,I_EOR,I_LSR,I_RMB,I_PHA,I_EOR,I_LSR,I_TAZ,I_JMP,I_EOR,I_LSR,I_BBR,
    I_BVC,I_EOR,I_EOR,I_BVC,I_ASR,I_EOR,I_LSR,I_RMB,I_CLI,I_EOR,I_PHY,I_TAB,I_MAP,I_EOR,I_LSR,I_BBR,
    I_RTS,I_ADC,I_RTS,I_BSR,I_STZ,I_ADC,I_ROR,I_RMB,I_PLA,I_ADC,I_ROR,I_TZA,I_JMP,I_ADC,I_ROR,I_BBR,
    I_BVS,I_ADC,I_ADC,I_BVS,I_STZ,I_ADC,I_ROR,I_RMB,I_SEI,I_ADC,I_PLY,I_TBA,I_JMP,I_ADC,I_ROR,I_BBR,
    I_BRA,I_STA,I_STA,I_BRA,I_STY,I_STA,I_STX,I_SMB,I_DEY,I_BIT,I_TXA,I_STY,I_STY,I_STA,I_STX,I_BBS,
    I_BCC,I_STA,I_STA,I_BCC,I_STY,I_STA,I_STX,I_SMB,I_TYA,I_STA,I_TXS,I_STX,I_STZ,I_STA,I_STZ,I_BBS,
    I_LDY,I_LDA,I_LDX,I_LDZ,I_LDY,I_LDA,I_LDX,I_SMB,I_TAY,I_LDA,I_TAX,I_LDZ,I_LDY,I_LDA,I_LDX,I_BBS,
    I_BCS,I_LDA,I_LDA,I_BCS,I_LDY,I_LDA,I_LDX,I_SMB,I_CLV,I_LDA,I_TSX,I_LDZ,I_LDY,I_LDA,I_LDX,I_BBS,
    I_CPY,I_CMP,I_CPZ,I_DEW,I_CPY,I_CMP,I_DEC,I_SMB,I_INY,I_CMP,I_DEX,I_ASW,I_CPY,I_CMP,I_DEC,I_BBS,
    I_BNE,I_CMP,I_CMP,I_BNE,I_CPZ,I_CMP,I_DEC,I_SMB,I_CLD,I_CMP,I_PHX,I_PHZ,I_CPZ,I_CMP,I_DEC,I_BBS,
    I_CPX,I_SBC,I_LDA,I_INW,I_CPX,I_SBC,I_INC,I_SMB,I_INX,I_SBC,I_EOM,I_ROW,I_CPX,I_SBC,I_INC,I_BBS,
    I_BEQ,I_SBC,I_SBC,I_BEQ,I_PHW,I_SBC,I_INC,I_SMB,I_SED,I_SBC,I_PLX,I_PLZ,I_PHW,I_SBC,I_INC,I_BBS,

    -- 6502 personality
    -- XXX Currently just a copy of 4502 personality
    I_BRK,I_ORA,I_KIL,I_SLO,I_NOP,I_ORA,I_ASL,I_SLO,I_PHP,I_ORA,I_ASL,I_ANC,I_NOP,I_ORA,I_ASL,I_SLO,
    I_BPL,I_ORA,I_KIL,I_SLO,I_NOP,I_ORA,I_ASL,I_SLO,I_CLC,I_ORA,I_NOP,I_SLO,I_NOP,I_ORA,I_ASL,I_SLO,
    I_JSR,I_AND,I_KIL,I_RLA,I_BIT,I_AND,I_ROL,I_RLA,I_PLP,I_AND,I_ROL,I_ANC,I_BIT,I_AND,I_ROL,I_RLA,
    I_BMI,I_AND,I_KIL,I_RLA,I_NOP,I_AND,I_ROL,I_RLA,I_SEC,I_AND,I_NOP,I_RLA,I_NOP,I_AND,I_ROL,I_RLA,
    I_RTI,I_EOR,I_KIL,I_SRE,I_NOP,I_EOR,I_LSR,I_SRE,I_PHA,I_EOR,I_LSR,I_ALR,I_JMP,I_EOR,I_LSR,I_SRE,
    I_BVC,I_EOR,I_KIL,I_SRE,I_NOP,I_EOR,I_LSR,I_SRE,I_CLI,I_EOR,I_NOP,I_SRE,I_NOP,I_EOR,I_LSR,I_SRE,
    I_RTS,I_ADC,I_KIL,I_RRA,I_NOP,I_ADC,I_ROR,I_RRA,I_PLA,I_ADC,I_ROR,I_ARR,I_JMP,I_ADC,I_ROR,I_RRA,
    I_BVS,I_ADC,I_KIL,I_RRA,I_NOP,I_ADC,I_ROR,I_RRA,I_SEI,I_ADC,I_NOP,I_RRA,I_NOP,I_ADC,I_ROR,I_RRA,
    I_NOP,I_STA,I_NOP,I_SAX,I_STY,I_STA,I_STX,I_SAX,I_DEY,I_NOP,I_TXA,I_XAA,I_STY,I_STA,I_STX,I_SAX,
    I_BCC,I_STA,I_KIL,I_AHX,I_STY,I_STA,I_STX,I_SAX,I_TYA,I_STA,I_TXS,I_TAS,I_SHY,I_STA,I_SHX,I_AHX,
    I_LDY,I_LDA,I_LDX,I_LAX,I_LDY,I_LDA,I_LDX,I_LAX,I_TAY,I_LDA,I_TAX,I_LAX,I_LDY,I_LDA,I_LDX,I_LAX,
    I_BCS,I_LDA,I_KIL,I_LAX,I_LDY,I_LDA,I_LDX,I_LAX,I_CLV,I_LDA,I_TSX,I_LAS,I_LDY,I_LDA,I_LDX,I_LAX,
    I_CPY,I_CMP,I_NOP,I_DCP,I_CPY,I_CMP,I_DEC,I_DCP,I_INY,I_CMP,I_DEX,I_AXS,I_CPY,I_CMP,I_DEC,I_DCP,
    I_BNE,I_CMP,I_KIL,I_DCP,I_NOP,I_CMP,I_DEC,I_DCP,I_CLD,I_CMP,I_NOP,I_DCP,I_NOP,I_CMP,I_DEC,I_DCP,
    I_CPX,I_SBC,I_NOP,I_ISC,I_CPX,I_SBC,I_INC,I_ISC,I_INX,I_SBC,I_NOP,I_SBC,I_CPX,I_SBC,I_INC,I_ISC,
    I_BEQ,I_SBC,I_KIL,I_ISC,I_NOP,I_SBC,I_INC,I_ISC,I_SED,I_SBC,I_NOP,I_ISC,I_NOP,I_SBC,I_INC,I_ISC
    );

  function disassemble_instruction(pc : unsigned(15 downto 0);
                                   bytes : instruction_bytes;
                                   cpu_mode : cpu_personality) return string;
end package;

package body disassemble is
  
  function disassemble_instruction(pc : unsigned(15 downto 0);
                                   bytes : instruction_bytes;
                                   cpu_mode : cpu_personality) return string is
      variable justification : side := RIGHT;
      variable size : width := 0;
      variable s : string(1 to 110) := (others => ' ');
      variable t : string(1 to 100) := (others => ' ');
      variable opcode : std_logic_vector(8 downto 0);
      variable branch_pc : unsigned(15 downto 0);
      variable offset : integer;
      variable addr_mode : addressing_mode;
  begin
--pragma synthesis_off    
    opcode(7 downto 0) := std_logic_vector(bytes.opcode);
    if cpu_mode = CPU6502 then
      opcode(8) := '1';
    else
      opcode(8) := '0';
    end if;

    t(1 to 5) := instruction'image(instruction_lut(to_integer(unsigned(opcode))));
    addr_mode := get_addressing_modes(opcode);


    offset := 1;

    s(offset to offset) := "$";
    offset := offset + 1;
    s(offset to (offset+3)) := to_hstring(pc);
    offset := offset + 4;
    s(offset to (offset+1)) := "  ";
    offset := offset + 2;
    
    s(offset to (offset+1)) := to_hstring(bytes.opcode);
    s((offset+2) to (offset+2)) := " ";
    offset := offset + 3;

    if addr_mode.imm8 or addr_mode.rel8 or addr_mode.rel16
      or addr_mode.addr8 or addr_mode.addr16 or addr_mode.imm16
      or addr_mode.rel8byte3 then
      s(offset to (offset+1)) := to_hstring(bytes.arg1);
      s((offset+2) to (offset+2)) := " ";
      offset := offset + 3;
      if addr_mode.imm16 or addr_mode.rel16 or addr_mode.addr16 then
        s(offset to (offset+1)) := to_hstring(bytes.arg2);
        s((offset+2) to (offset+2)) := " ";
        offset := offset + 3;
      else
        s(offset to (offset+2)) := "   ";
        offset := offset + 3;
      end if;
    else
      s(offset to (offset+5)) := "      ";
      offset := offset + 6;
    end if;


    -- Space before instruction name
    s(offset to offset) := " ";
    offset := offset + 1;

    -- Actual instruction name
    s(offset to (offset+2)) := t(3 to 5);
    offset := offset + 3;
      
    -- Draw 0-7 digit on BBS/BBR instructions
    case instruction_lut(to_integer(unsigned(opcode))) is
      when I_BBS =>
        s(offset to offset) := to_hstring("0"&bytes.opcode(6 downto 4))(1 to 1);
        offset := offset + 1;
      when I_BBR =>
        s(offset to offset) := to_hstring("0"&bytes.opcode(6 downto 4))(1 to 1);
        offset := offset + 1;
      when others =>
        null;
    end case;

    -- Space after instruction name
    s(offset to offset) := " ";
    offset := offset + 1;
    
    if addr_mode.indirect then
      s(offset to offset) := "(";
      offset := offset + 1;
    end if;
    if addr_mode.imm8 or addr_mode.imm16 then
      s(offset to offset) := "#";
      offset := offset + 1;
    end if;
    if addr_mode.a then
      s(offset to offset) := "A";
      offset := offset + 1;
    elsif addr_mode.imm8 or addr_mode.rel8 or addr_mode.rel16
      or addr_mode.addr8 or addr_mode.addr16 or addr_mode.imm16
      or addr_mode.rel8byte3 then
      s(offset to offset) := "$";
      offset := offset + 1;
    end if;
    if addr_mode.imm16 or addr_mode.addr16 then
      s(offset to (offset+1)) := to_hstring(bytes.arg2)(1 to 2);
      offset := offset + 2;
    end if;
    if addr_mode.imm8 or addr_mode.addr8 or addr_mode.rel8byte3
       or addr_mode.imm16 or addr_mode.addr16 then
      s(offset to (offset+1)) := to_hstring(bytes.arg1)(1 to 2);
      offset := offset + 2;
    end if;
    if addr_mode.prex then
      s(offset to (offset+1)) := ",X";
      offset := offset + 2;
    end if;
    if addr_mode.presp then
      s(offset to (offset+2)) := ",SP";
      offset := offset + 3;
    end if;
    if addr_mode.indirect then
      s(offset to offset) := ")";
      offset := offset + 1;
    end if;
    if addr_mode.rel8byte3 then
      s(offset to offset +1 ) := ",$";
      offset := offset + 2;
    end if;
    branch_pc :=
      to_unsigned(65539 + to_integer(pc) + to_integer(bytes.arg2&bytes.arg1),16);
    if addr_mode.rel8byte3 then
      branch_pc :=
        to_unsigned(65538 + to_integer(pc) + to_integer(
          bytes.arg2(7)&bytes.arg2(7)&bytes.arg2(7)&bytes.arg2(7)&
          bytes.arg2(7)&bytes.arg2(7)&bytes.arg2(7)&bytes.arg2(7)&
          bytes.arg2),16);
    end if;
    if addr_mode.rel8 then
      branch_pc :=
        to_unsigned(65539 + to_integer(pc) + to_integer(
          bytes.arg1(7)&bytes.arg1(7)&bytes.arg1(7)&bytes.arg1(7)&
          bytes.arg1(7)&bytes.arg1(7)&bytes.arg1(7)&bytes.arg1(7)&
          bytes.arg1),16);
    end if;
    if addr_mode.rel8 or addr_mode.rel16 or addr_mode.rel8byte3 then
      s(offset to offset+3) := to_hstring(branch_pc);
      offset := offset + 4;
    end if;
    if addr_mode.postx then
      s(offset to (offset+1)) := ",X";
      offset := offset + 2;
    end if;
    if addr_mode.posty then
      s(offset to (offset+1)) := ",Y";
      offset := offset + 2;
    end if;
    if addr_mode.postz then
      s(offset to (offset+1)) := ",Z";
      offset := offset + 2;
    end if;

    return s(1 to offset-1);
--pragma synthesis_on
  end function;
end package body;
  
