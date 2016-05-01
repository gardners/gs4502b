#include <stdio.h>
#include <stdlib.h>

struct flag_name {
  char *name;
  long long value;
};

#include "extra_instruction_flags.h"

#define I_LDAIMM NZ_FROM_I2|LDA|REG_OP
#define I_LDXIMM NZ_FROM_I2|LDX|REG_OP
#define I_LAXIMM NZ_FROM_I2|LDX|LDA|REG_OP
#define I_LDYIMM NZ_FROM_I2|LDY|REG_OP
#define I_LDZIMM NZ_FROM_I2|LDZ|REG_OP
#define I_LDA NOTHING
#define I_LDX NOTHING
#define I_LAX NOTHING
#define I_LDY NOTHING
#define I_LDZ NOTHING
#define I_STA NOTHING
#define I_STX NOTHING
#define I_SAX NOTHING
#define I_STY NOTHING
#define I_STZ NOTHING

#define I_ADC NZ_FROM_ALU|IS_ALU_OP
#define I_SBC NZ_FROM_ALU|IS_ALU_OP
#define I_AND NZ_FROM_ALU|IS_ALU_OP
#define I_EOR NZ_FROM_ALU|IS_ALU_OP
#define I_BIT NZ_FROM_ALU
#define I_ORA NZ_FROM_ALU|IS_ALU_OP

#define I_CMP NZ_FROM_ALU|IS_ALU_OP
#define I_CPX NZ_FROM_ALU|IS_ALU_OP|CPX|COMPARE_INDEX_REGISTER
#define I_CPY NZ_FROM_ALU|IS_ALU_OP|CPY|COMPARE_INDEX_REGISTER
#define I_CPZ NZ_FROM_ALU|IS_ALU_OP|CPZ|COMPARE_INDEX_REGISTER

#define I_CLE NOTHING
#define I_SEE NOTHING
#define I_CLD NOTHING
#define I_SED NOTHING
#define I_CLC NOTHING
#define I_SEC NOTHING
#define I_CLI NOTHING
#define I_SEI NOTHING
#define I_CLV NOTHING

#define I_PHA NOTHING
#define I_PHP NOTHING
#define I_PHX NOTHING
#define I_PHY NOTHING
#define I_PHZ NOTHING

#define I_PLA NOTHING
#define I_PLP NOTHING
#define I_PLX NOTHING
#define I_PLY NOTHING
#define I_PLZ NOTHING

#define I_BNE NOTHING
#define I_BEQ NOTHING
#define I_BPL NOTHING
#define I_BMI NOTHING
#define I_BCC NOTHING
#define I_BCS NOTHING
#define I_BVC NOTHING
#define I_BVS NOTHING
#define I_BBR NOTHING
#define I_BBS NOTHING
// we assume that pc_expected is branch address, pc_mispredict is return address,
// so we JMP/BRA are effectively NOPs, while JSR/BSR just have to push the PC to stack.
#define I_JSR NOTHING
#define I_BSR NOTHING
#define I_BRA NOTHING
#define I_JMP NOTHING
#define I_BRK NOTHING
#define I_RTS NOTHING
#define I_RTI NOTHING
// We will use the same flag for MAP and EOM (NOP), and check the opcode to discern between them.
#define I_MAP NOTHING
#define I_EOM NOTHING
#define I_NOP NOTHING

// Register increment/decrements
#define I_NEG NEGA|REG_OP
#define I_INCA INCA|NZ_FROM_A|REG_OP
#define I_DECA DECA|NZ_FROM_A|REG_OP
#define I_DEX DECX|NZ_FROM_X|REG_OP
#define I_INX INCX|NZ_FROM_X|REG_OP
#define I_DEY DECY|NZ_FROM_Y|REG_OP
#define I_INY INCY|NZ_FROM_Y|REG_OP
#define I_DEZ DECZ|NZ_FROM_Z|REG_OP
#define I_INZ INCZ|NZ_FROM_Z|REG_OP
// Accumulator RMW instructions
#define I_ASLA NZ_FROM_ALU|IS_ALU_OP
#define I_ROLA NZ_FROM_ALU|IS_ALU_OP
#define I_LSRA NZ_FROM_ALU|IS_ALU_OP
#define I_ASRA NZ_FROM_ALU|IS_ALU_OP
#define I_RORA NZ_FROM_ALU|IS_ALU_OP

// Memory RMW instructions
#define I_INC NOTHING
#define I_DEC NOTHING
#define I_ASL NOTHING
#define I_ROL NOTHING
#define I_LSR NOTHING
#define I_ASR NOTHING
#define I_ROR NOTHING
// New RMW instructions for bit fiddling
#define I_TSB NOTHING
#define I_TRB NOTHING
#define I_RMB NOTHING
#define I_SMB NOTHING

#define I_TAB TAB|REG_OP
#define I_TAX TAX|NZ_FROM_A|REG_OP
#define I_TAY TAY|NZ_FROM_A|REG_OP
#define I_TAZ TAZ|NZ_FROM_A|REG_OP
#define I_TBA TBA|NZ_FROM_B|REG_OP
#define I_TXA TXA|NZ_FROM_X|REG_OP
#define I_TYA TYA|NZ_FROM_Y|REG_OP
#define I_TZA TZA|NZ_FROM_Z|REG_OP

#define I_TYS TYS|REG_OP
#define I_TSY TSY|NZ_FROM_SPH|REG_OP
#define I_TXS TXS|REG_OP
#define I_TSX TSX|NZ_FROM_SPL|REG_OP

// The funny word-based operations. For now, we will trigger an exception,
// and have the hypervisor implement them?
#define I_DEW NOTHING
#define I_ASW NOTHING
#define I_INW NOTHING
#define I_ROW NOTHING
#define I_PHW NOTHING
// And do the same with the 6502 KIL instruction
#define I_KIL NOTHING

// And also the 6502 illegal opcodes. We will implement these though, as they
// should just be simple combinations of ALU operations.
#define I_RLA NOTHING
#define I_SLO NOTHING
#define I_ALR NOTHING
#define I_ANC NOTHING
#define I_SRE NOTHING
#define I_RRA NOTHING
#define I_ARR NOTHING
#define I_XAA NOTHING
#define I_AHX NOTHING
#define I_TAS NOTHING
#define I_SHY NOTHING
#define I_SHX NOTHING
#define I_DCP NOTHING
#define I_LAS NOTHING
#define I_AXS NOTHING
#define I_ISC NOTHING


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
    I_LDYIMM,I_LDA,I_LDXIMM,I_LDZIMM,I_LDY,I_LDA,I_LDX,I_SMB,I_TAY,I_LDAIMM,I_TAX,I_LDZ,I_LDY,I_LDA,I_LDX,I_BBS,
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
    I_LDYIMM,I_LDA,I_LDXIMM,I_LAXIMM,I_LDY,I_LDA,I_LDX,I_LAX,I_TAY,I_LDAIMM,I_TAX,I_LAX,I_LDY,I_LDA,I_LDX,I_LAX,
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
  printf("Generating extra_instruction_equations.vhdl\n");
  FILE *f=fopen("extra_instruction_equations.vhdl","w");
  fprintf(f,
	  "-- DO NOT MODIFY: Auto-generated by extrainstructionflags.c\n"
	  "\n\n"
	  "library ieee;\n"
	  "use Std.TextIO.all;\n"
	  "use ieee.STD_LOGIC_1164.all;\n"
	  "use ieee.numeric_std.all;\n"
	  "\n"
	  "package extra_instruction_equations is\n"
	  "\n"
	  "  type extra_instruction_flags is record\n");	  
  for(long long flag=1;flag<=MAX_FLAG;flag*=2) {
    fprintf(f,"    %s : boolean;\n",flagname(flag));
  }
  fprintf(f,
          "  end record;\n"
	  "\n"
	  "  function get_extra_instruction_flags(opcode : std_logic_vector(8 downto 0)) return extra_instruction_flags;\n"
	  "\n"
	  "end package;\n"
	  "\n"
	  "package body extra_instruction_equations is\n"
	  "  function get_extra_instruction_flags(opcode : std_logic_vector(8 downto 0)) return extra_instruction_flags is\n"
	  "    variable mode : extra_instruction_flags := (others => false);\n"
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
