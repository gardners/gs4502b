#include <stdio.h>
#include <stdlib.h>

struct flag_name {
  char *name;
  long long value;
};

#include "instruction_flags.h"

#define I_LDA DO_LOAD|ALUSRC_M|ALUDST_A|UPDATE_NZ|ALU_SET
#define I_LDX DO_LOAD|ALUSRC_M|ALUDST_X|UPDATE_NZ|ALU_SET
#define I_LAX DO_LOAD|ALUSRC_M|ALUDST_A|ALUDST_X|UPDATE_NZ|ALU_SET
#define I_LDY DO_LOAD|ALUSRC_M|ALUDST_Y|UPDATE_NZ|ALU_SET
#define I_LDZ DO_LOAD|ALUSRC_M|ALUDST_Z|UPDATE_NZ|ALU_SET
#define I_STA DO_STORE|ALUSRC_A|UPDATE_NZ
#define I_STX DO_STORE|ALUSRC_X|UPDATE_NZ
#define I_SAX DO_STORE|ALUSRC_A|ALUSRC_X|UPDATE_NZ
#define I_STY DO_STORE|ALUSRC_Y|UPDATE_NZ
#define I_STZ DO_STORE|ALUSRC_Z|UPDATE_NZ

#define I_ADC ALU_ADC|ALUSRC_A|ALUDST_A|UPDATE_NZ|UPDATE_C|UPDATE_V
#define I_SBC ALU_SBC|ALUSRC_A|ALUDST_A|UPDATE_NZ|UPDATE_C|UPDATE_V
#define I_AND ALU_AND|ALUSRC_A|ALUDST_A|UPDATE_NZ
#define I_EOR ALU_EOR|ALUSRC_A|ALUDST_A|UPDATE_NZ
#define I_BIT ALU_AND|ALUSRC_A|UPDATE_NZ
#define I_ORA ALU_OR|ALUSRC_A|ALUDST_A|UPDATE_NZ

#define I_CMP ALU_CMP|ALUSRC_A|UPDATE_NZ|UPDATE_C|UPDATE_V
#define I_CPX ALU_CMP|ALUSRC_X|UPDATE_NZ|UPDATE_C|UPDATE_V
#define I_CPY ALU_CMP|ALUSRC_Y|UPDATE_NZ|UPDATE_C|UPDATE_V
#define I_CPZ ALU_CMP|ALUSRC_Z|UPDATE_NZ|UPDATE_C|UPDATE_V

#define I_CLE DO_CLEAR_FLAG|DO_SET_E
#define I_SEE DO_SET_FLAG|DO_SET_E
#define I_CLD DO_CLEAR_FLAG|DO_SET_D
#define I_SED DO_SET_FLAG|DO_SET_D
#define I_CLC DO_CLEAR_FLAG|DO_SET_C
#define I_SEC DO_SET_FLAG|DO_SET_C
#define I_CLI DO_CLEAR_FLAG|DO_SET_I
#define I_SEI DO_SET_FLAG|DO_SET_I
#define I_CLV DO_CLEAR_FLAG|DO_SET_V

#define I_PHA DO_PUSH|ALUSRC_A
#define I_PHP DO_PUSH|ALUSRC_P
#define I_PHX DO_PUSH|ALUSRC_X
#define I_PHY DO_PUSH|ALUSRC_Y
#define I_PHZ DO_PUSH|ALUSRC_Z

#define I_PLA DO_PULL|ALUSRC_M|ALUDST_A|UPDATE_NZ
#define I_PLP DO_PULL|ALUSRC_M|ALUDST_P
#define I_PLX DO_PULL|ALUSRC_M|ALUDST_X|UPDATE_NZ
#define I_PLY DO_PULL|ALUSRC_M|ALUDST_Y|UPDATE_NZ
#define I_PLZ DO_PULL|ALUSRC_M|ALUDST_Z|UPDATE_NZ

#define I_BNE DO_BRANCH|BRANCH_ON_CLEAR|BRANCH_Z|DO_BRANCH_CONDITIONAL
#define I_BEQ DO_BRANCH|BRANCH_Z|DO_BRANCH_CONDITIONAL
#define I_BPL DO_BRANCH|BRANCH_ON_CLEAR|BRANCH_N|DO_BRANCH_CONDITIONAL
#define I_BMI DO_BRANCH|BRANCH_N|DO_BRANCH_CONDITIONAL
#define I_BCC DO_BRANCH|BRANCH_ON_CLEAR|BRANCH_C|DO_BRANCH_CONDITIONAL
#define I_BCS DO_BRANCH|BRANCH_C|DO_BRANCH_CONDITIONAL
#define I_BVC DO_BRANCH|BRANCH_ON_CLEAR|BRANCH_V|DO_BRANCH_CONDITIONAL
#define I_BVS DO_BRANCH|BRANCH_V|DO_BRANCH_CONDITIONAL
#define I_BBR DO_BRANCH|BRANCH_ON_CLEAR|BRANCH_ON_ZPBIT
#define I_BBS DO_BRANCH|BRANCH_ON_ZPBIT
// we assume that pc_expected is branch address, pc_mispredict is return address,
// so we JMP/BRA are effectively NOPs, while JSR/BSR just have to push the PC to stack.
#define I_JSR DO_SUBROUTINE|DO_BRANCH
#define I_BSR DO_SUBROUTINE|DO_BRANCH
#define I_BRA DO_BRANCH
#define I_JMP DO_BRANCH
#define I_BRK DO_INTERRUPT
#define I_RTS DO_RETURN
#define I_RTI DO_PULL|ALUSRC_M|ALUDST_P|DO_RETURN
// We will use the same flag for MAP and EOM (NOP), and check the opcode to discern between them.
#define I_MAP DO_MAP
#define I_EOM DO_MAP
#define I_NOP DO_MAP

// Register increment/decrements
#define I_NEG ALUSRC_A|ALU_NEG|ALUDST_A|UPDATE_NZ
#define I_INCA ALUSRC_A|ALU_INC|ALUDST_A|UPDATE_NZ
#define I_DECA ALUSRC_A|ALU_INC|ALUDST_A|UPDATE_NZ
#define I_DEX ALUSRC_X|ALU_DEC|ALUDST_X|UPDATE_NZ
#define I_INX ALUSRC_X|ALU_INC|ALUDST_X|UPDATE_NZ
#define I_DEY ALUSRC_Y|ALU_DEC|ALUDST_Y|UPDATE_NZ
#define I_INY ALUSRC_Y|ALU_INC|ALUDST_Y|UPDATE_NZ
#define I_DEZ ALUSRC_Z|ALU_DEC|ALUDST_Z|UPDATE_NZ
#define I_INZ ALUSRC_Z|ALU_INC|ALUDST_Z|UPDATE_NZ
// Accumulator RMW instructions
#define I_ASLA DO_ALUOP|ALU_ASL|UPDATE_NZ|UPDATE_C
#define I_ROLA DO_ALUOP|ALU_ASL|UPDATE_NZ|UPDATE_C|IS_ROTATE
#define I_LSRA DO_ALUOP|ALU_LSR|UPDATE_NZ|UPDATE_C
#define I_ASRA DO_ALUOP|ALU_LSR|UPDATE_NZ|UPDATE_C|IS_SIGNEXTEND
#define I_RORA DO_ALUOP|ALU_LSR|UPDATE_NZ|UPDATE_C|IS_ROTATE

// Memory RMW instructions
#define I_INC DO_LOAD|DO_STORE|ALU_ASL|UPDATE_NZ
#define I_DEC DO_LOAD|DO_STORE|ALU_ASL|UPDATE_NZ
#define I_ASL DO_LOAD|DO_STORE|ALU_ASL|UPDATE_NZ|UPDATE_C
#define I_ROL DO_LOAD|DO_STORE|ALU_ASL|UPDATE_NZ|UPDATE_C|IS_ROTATE
#define I_LSR DO_LOAD|DO_STORE|ALU_LSR|UPDATE_NZ|UPDATE_C
#define I_ASR DO_LOAD|DO_STORE|ALU_LSR|UPDATE_NZ|UPDATE_C|IS_SIGNEXTEND
#define I_ROR DO_LOAD|DO_STORE|ALU_LSR|UPDATE_NZ|UPDATE_C|IS_ROTATE
// New RMW instructions for bit fiddling
#define I_TSB DO_LOAD|DO_STORE|ALU_TSB|ALUSRC_A
#define I_TRB DO_LOAD|DO_STORE|ALU_TSB|ALUSRC_A
#define I_RMB DO_LOAD|DO_STORE|ALU_RMB|ALUSRC_RMB_OR_SMB_OPCODE_BITS
#define I_SMB DO_LOAD|DO_STORE|ALU_RMB|ALUSRC_RMB_OR_SMB_OPCODE_BITS

#define I_TAB ALUSRC_A|ALUDST_B
#define I_TAX ALUSRC_A|ALUDST_X|UPDATE_NZ
#define I_TAY ALUSRC_A|ALUDST_Y|UPDATE_NZ
#define I_TAZ ALUSRC_A|ALUDST_Z|UPDATE_NZ
#define I_TBA ALUSRC_B|ALUDST_A|UPDATE_NZ
#define I_TXA ALUSRC_X|ALUDST_A|UPDATE_NZ
#define I_TYA ALUSRC_Y|ALUDST_A|UPDATE_NZ
#define I_TZA ALUSRC_Z|ALUDST_A|UPDATE_NZ

#define I_TYS ALUSRC_Y|ALUDST_SPH
#define I_TSY ALUSRC_SPH|ALUDST_Y|UPDATE_NZ
#define I_TXS ALUSRC_X|ALUDST_SPL
#define I_TSX ALUSRC_SPL|ALUDST_X|UPDATE_NZ

// The funny word-based operations. For now, we will trigger an exception,
// and have the hypervisor implement them?
#define I_DEW DO_EXCEPTION
#define I_ASW DO_EXCEPTION
#define I_INW DO_EXCEPTION
#define I_ROW DO_EXCEPTION
#define I_PHW DO_EXCEPTION
// And do the same with the 6502 KIL instruction
#define I_KIL DO_EXCEPTION

// And also the 6502 illegal opcodes. We will implement these though, as they
// should just be simple combinations of ALU operations.
#define I_RLA DO_EXCEPTION
#define I_SLO DO_EXCEPTION
#define I_ALR DO_EXCEPTION
#define I_ANC DO_EXCEPTION
#define I_SRE DO_EXCEPTION
#define I_RRA DO_EXCEPTION
#define I_ARR DO_EXCEPTION
#define I_XAA DO_EXCEPTION
#define I_AHX DO_EXCEPTION
#define I_TAS DO_EXCEPTION
#define I_SHY DO_EXCEPTION
#define I_SHX DO_EXCEPTION
#define I_DCP DO_EXCEPTION
#define I_LAS DO_EXCEPTION
#define I_AXS DO_EXCEPTION
#define I_ISC DO_EXCEPTION


char *flagname(long long flag) {
  for(int i=0;flag_names[i].name;i++)
    if (flag==flag_names[i].value) return flag_names[i].name;
  fprintf(stderr,"unknown flag $%04llx\n",flag);
  exit(-1);
}

// XXX - Mark all Accumulator mode instructions, so that they can be marked non-load
// XXX - Mark all Immediate mode instructions, so that they can be marked non-load
// XXX   (like I_INCA and I_DECA for the accumulator mode of INC/DEC)
long long opcodes[512]={
  // 4502 personality
    I_BRK,I_ORA,I_CLE,I_SEE,I_TSB,I_ORA,I_ASL,I_RMB,I_PHP,I_ORA,I_ASL,I_TSY,I_TSB,I_ORA,I_ASL,I_BBR,
    I_BPL,I_ORA,I_ORA,I_BPL,I_TRB,I_ORA,I_ASL,I_RMB,I_CLC,I_ORA,I_INCA,I_INZ,I_TRB,I_ORA,I_ASL,I_BBR,
    I_JSR,I_AND,I_JSR,I_JSR,I_BIT,I_AND,I_ROL,I_RMB,I_PLP,I_AND,I_ROL,I_TYS,I_BIT,I_AND,I_ROL,I_BBR,
    I_BMI,I_AND,I_AND,I_BMI,I_BIT,I_AND,I_ROL,I_RMB,I_SEC,I_AND,I_DECA,I_DEZ,I_BIT,I_AND,I_ROL,I_BBR,
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

  // -- 6502 personality
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
};
int covered[512]={0};

int masks[512];

int bitsset(long long v)
{
  int count=0;
  for(long long i=1;i<(1LL<<63);i*=2)
    if (v&i) count++;
  return count;
}

int compare_bitcounts(const void *aa, const void *bb)
{
  int a=*(int *)aa;
  int b=*(int *)bb;
  if (bitsset(a)<bitsset(b)) return -1;
  if (bitsset(a)>bitsset(b)) return 1;
  return 0;
}

struct rule {
  int m;
  int v;
};

#define MAX_RULES 512
int rule_count=0;
struct rule rules[MAX_RULES];

int main()
{
  // Work out all possible masks
  for(int i=0;i<512;i++) masks[i]=i+1;
  qsort(masks,512,sizeof(int),compare_bitcounts);

  // Okay, we now have a set of rules that we can apply.
  // Generate VHDL file
  printf("Generating instruction_equations.vhdl\n");
  FILE *f=fopen("instruction_equations.vhdl","w");
  fprintf(f,
	  "-- DO NOT MODIFY: Auto-generated by instructionequations.c\n"
	  "\n\n"
	  "library ieee;\n"
	  "use Std.TextIO.all;\n"
	  "use ieee.STD_LOGIC_1164.all;\n"
	  "use ieee.numeric_std.all;\n"
	  "\n"
	  "package instruction_equations is\n"
	  "\n"
	  "  type instruction_flags is record\n");	  
  for(long long flag=1;flag<=MAX_FLAG;flag*=2) {
    fprintf(f,"    %s : boolean;\n",flagname(flag));
  }
  fprintf(f,
          "  end record;\n"
	  "\n"
	  "  function get_instruction_flags(opcode : std_logic_vector(8 downto 0)) return instruction_flags;\n"
	  "\n"
	  "end package;\n"
	  "\n"
	  "package body instruction_equations is\n"
	  "  function get_instruction_flags(opcode : std_logic_vector(8 downto 0)) return instruction_flags is\n"
	  "    variable mode : instruction_flags := (others => false);\n"
	  "  begin\n"
	  );
  
  for(long long flag=1;flag<=MAX_FLAG;flag*=2) {
    
    // Try all masks with all possible values, to see what the largest mask is
    // that we can use to cover the most unclassified remaining opcodes for this mode action.

    rule_count=0;
    
    int remaining=512;
    for(int i=0;i<512;i++) {
      covered[i]=0;
      if (!(opcodes[i]&flag)) {
	covered[i]=1; remaining--;
      }
    }
    printf("Finding equations for %d opcodes with mode.%s\n",
	   remaining,flagname(flag));
  
    while(remaining>0) {  
      int best_matches=-1;
      int best_m=-1, best_v=-1;
      for(int m=0;m<512;m++)
	{
	  int last_mv=-1;
	  for(int v=0;v<512;v++) {
	    int mv=m&v;
	    if (mv!=last_mv) {
	      int matches=0;
	      for(int i=0;i<512;i++) {
		if ((i&m)==v) {
		  if (!(opcodes[i]&flag)) { matches=-1; break; }
		  if (!covered[i]) matches++;
		}
	      }
	      if (matches>best_matches) {
		best_m=m; best_v=v;
		best_matches=matches;
	      }
	      last_mv=mv;
	    }
	  }
	}
      printf("mode.%s, Rule #%d: OPCODE & $%03x = $%03x -> flag is %s (covers %d/%d remaining instructions).\n",
	     flagname(flag),rule_count,best_m,best_v,flagname(flag),best_matches,remaining);
      rules[rule_count].m=best_m;
      rules[rule_count].v=best_v;
      rule_count++;
      for(int i=0;i<512;i++) if ((i&best_m)==best_v) { if (!covered[i]) remaining--; covered[i]=1; }
    }
    
    // Now verify rules
    printf("Verifying that %d rules correctly classify lengths of all instructions.\n",
	   rule_count);
    for(int i=0;i<512;i++) {
      int actual_value=(opcodes[i]&flag)?1:0;
      int predicted_value=0;
      for(int r=0;r<rule_count;r++)
	{
	  if ((i&rules[r].m)==rules[r].v) {
	    predicted_value=1;
	  }
	}
      if (predicted_value==-1) predicted_value=0;
      if (predicted_value!=actual_value) {
	printf("Rules produced wrong value for $%03x: expected %d, but saw %d\n",
	       i,actual_value,predicted_value);
	exit(-1);
      }
    }

    fprintf(f,"    -- Equations for mode.%s\n",flagname(flag));
    for(int r=0;r<rule_count;r++) {
      fprintf(f,"    if (opcode and \"%c%c%c%c%c%c%c%c%c\") = \"%c%c%c%c%c%c%c%c%c\" then mode.%s := true; end if;\n",
	      rules[r].m&256?'1':'0',
	      rules[r].m&128?'1':'0',
	      rules[r].m&64?'1':'0',
	      rules[r].m&32?'1':'0',
	      rules[r].m&16?'1':'0',
	      rules[r].m&8?'1':'0',
	      rules[r].m&4?'1':'0',
	      rules[r].m&2?'1':'0',
	      rules[r].m&1?'1':'0',
	      rules[r].v&256?'1':'0',
	      rules[r].v&128?'1':'0',
	      rules[r].v&64?'1':'0',
	      rules[r].v&32?'1':'0',
	      rules[r].v&16?'1':'0',
	      rules[r].v&8?'1':'0',
	      rules[r].v&4?'1':'0',
	      rules[r].v&2?'1':'0',
	      rules[r].v&1?'1':'0',
	      flagname(flag)
	      );
    }    
  }
  
  fprintf(f,
	  "    return mode;\n"
	  "  end function;\n"
	  "end package body;\n");
  fclose(f);
  return 0;
  
}
