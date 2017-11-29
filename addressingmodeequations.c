#include <stdio.h>
#include <stdlib.h>

#define SRC_A 1
#define SRC_IMM 2
#define REL_8 4
#define REL_16 8
#define INDIRECT 16
#define PREINDEX_X 32
#define PREINDEX_SP 64
#define POSTINDEX_X 128
#define POSTINDEX_Y 256
#define POSTINDEX_Z 512
#define ZP 1024
#define ABS 2048
#define SRC_IMM16 4096
#define REL_8_3RDBYTE 8192

#define MAX_FLAG 8192

char *flagname(int flag) {
  switch(flag) {
  case SRC_A: return "a";
  case SRC_IMM: return "imm8";
  case REL_8: return "rel8";
  case REL_16: return "rel16";
  case INDIRECT: return "indirect";
  case PREINDEX_X: return "prex";
  case PREINDEX_SP: return "presp";
  case POSTINDEX_X: return "postx";
  case POSTINDEX_Y: return "posty";
  case POSTINDEX_Z: return "postz";
  case ZP: return "addr8";
  case ABS: return "addr16";
  case SRC_IMM16: return "imm16";
  case REL_8_3RDBYTE: return "rel8byte3";
  default:
    fprintf(stderr,"unknown flag $%04x\n",flag);
    exit(-1);
  }
}


#define M_impl 0
#define M_InnX INDIRECT|ZP|PREINDEX_X
#define M_nn ZP
#define M_A SRC_A
#define M_rr REL_8
#define M_InnY INDIRECT|ZP|POSTINDEX_Y
#define M_InnZ INDIRECT|ZP|POSTINDEX_Z
#define M_rrrr REL_16
#define M_InnnnX INDIRECT|ZP|PREINDEX_X
#define M_nnnn ABS
#define M_nnnnX ABS|POSTINDEX_X
#define M_nnnnY ABS|POSTINDEX_Y
#define M_immnnnn SRC_IMM16
#define M_nnrr ZP|REL_8_3RDBYTE
#define M_immnn SRC_IMM
#define M_nnX ZP|POSTINDEX_X
#define M_nnY ZP|POSTINDEX_Y
#define M_Innnn INDIRECT|ABS
#define M_InnSPY INDIRECT|PREINDEX_SP|POSTINDEX_Y

int modes[512]={
  // -- 4502 personality first
  // -- BRK advances PC by two, not one, so we mark it as immediate mode
  M_immnn,  M_InnX,  M_impl,  M_impl,  M_nn,    M_nn,    M_nn,    M_nn,    
  M_impl,  M_immnn, M_A,     M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
  M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nn,    M_nnX,   M_nnX,   M_nn,    
  M_impl,  M_nnnnY, M_impl,  M_impl,  M_nnnn,  M_nnnnX, M_nnnnX, M_nnrr,  
  M_nnnn,  M_InnX,  M_Innnn, M_InnnnX,M_nn,    M_nn,    M_nn,    M_nn,    
  M_impl,  M_immnn, M_A,     M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
  M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnX,   M_nn,    
  M_impl,  M_nnnnY, M_impl,  M_impl,  M_nnnnX, M_nnnnX, M_nnnnX, M_nnrr,  
  M_impl,  M_InnX,  M_impl,  M_impl,  M_nn,    M_nn,    M_nn,    M_nn,    
  M_impl,  M_immnn, M_A,     M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
  M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnX,   M_nn,    
  M_impl,  M_nnnnY, M_impl,  M_impl,  M_impl,  M_nnnnX, M_nnnnX, M_nnrr,
  //   -- $63 BSR $nnnn is 16-bit relative on the 4502.  We treat it as absolute
  // -- mode, with microcode being used to select relative addressing.
  M_impl,  M_InnX,  M_immnn, M_nnnn,  M_nn,    M_nn,    M_nn,    M_nn,    
  M_impl,  M_immnn, M_A,     M_impl,  M_Innnn, M_nnnn,  M_nnnn,  M_nnrr,  
  M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnX,   M_nn,    
  M_impl,  M_nnnnY, M_impl,  M_impl,  M_InnnnX,M_nnnnX, M_nnnnX, M_nnrr,  
  M_rr,    M_InnX,  M_InnSPY,M_rrrr,  M_nn,    M_nn,    M_nn,    M_nn,    
  M_impl,  M_immnn, M_impl,  M_nnnnX, M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
  M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnY,   M_nn,    
  M_impl,  M_nnnnY, M_impl,  M_nnnnY, M_nnnn,  M_nnnnX, M_nnnnX, M_nnrr,  
  M_immnn, M_InnX,  M_immnn, M_immnn, M_nn,    M_nn,    M_nn,    M_nn,    
  M_impl,  M_immnn, M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
  M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnY,   M_nn,
  M_impl,  M_nnnnY, M_impl,  M_nnnnX, M_nnnnX, M_nnnnX, M_nnnnY, M_nnrr,  
  M_immnn, M_InnX,  M_immnn, M_nn,    M_nn,    M_nn,    M_nn,    M_nn,    
  M_impl,  M_immnn, M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
  M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nn,    M_nnX,   M_nnX,   M_nn,
  M_impl,  M_nnnnY, M_impl,  M_impl,  M_nnnn,  M_nnnnX, M_nnnnX, M_nnrr,  
  M_immnn, M_InnX,  M_InnSPY,M_nn,    M_nn,    M_nn,    M_nn,    M_nn,    
  M_impl,  M_immnn, M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
  M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_immnnnn,M_nnX,   M_nnX,   M_nn,    
  M_impl,  M_nnnnY, M_impl,  M_impl,  M_nnnn,  M_nnnnX, M_nnnnX, M_nnrr,

  // -- 6502 personality
  // -- XXX currently just a copy of 4502 personality
  // -- BRK advances PC by two, not one, so we mark it as immediate mode
  M_immnn,M_InnX,M_impl,M_InnX,M_nn,M_nn,M_nn,M_nn,
  M_impl,M_immnn,M_impl,M_immnn,M_nnnn,M_nnnn,M_nnnn,M_nnnn,
  M_rr,M_InnY,M_impl,M_InnY,M_nnX,M_nnX,M_nnX,M_nnX,
  M_impl,M_nnnnY,M_impl,M_nnnnY,M_nnnnX,M_nnnnX,M_nnnnX,M_nnnnX,
  M_nnnn,M_InnX,M_impl,M_InnX,M_nn,M_nn,M_nn,M_nn,
  M_impl,M_immnn,M_impl,M_immnn,M_nnnn,M_nnnn,M_nnnn,M_nnnn,
  M_rr,M_InnY,M_impl,M_InnY,M_nnX,M_nnX,M_nnX,M_nnX,
  M_impl,M_nnnnY,M_impl,M_nnnnY,M_nnnnX,M_nnnnX,M_nnnnX,M_nnnnX,
  M_impl,M_InnX,M_impl,M_InnX,M_nn,M_nn,M_nn,M_nn,
  M_impl,M_immnn,M_impl,M_immnn,M_nnnn,M_nnnn,M_nnnn,M_nnnn,
  M_rr,M_InnY,M_impl,M_InnY,M_nnX,M_nnX,M_nnX,M_nnX,
  M_impl,M_nnnnY,M_impl,M_nnnnY,M_nnnnX,M_nnnnX,M_nnnnX,M_nnnnX,
  M_impl,M_InnX,M_impl,M_InnX,M_nn,M_nn,M_nn,M_nn,
  M_impl,M_immnn,M_impl,M_immnn,M_Innnn,M_nnnn,M_nnnn,M_nnnn,
  M_rr,M_InnY,M_impl,M_InnY,M_nnX,M_nnX,M_nnX,M_nnX,
  M_impl,M_nnnnY,M_impl,M_nnnnY,M_nnnnX,M_nnnnX,M_nnnnX,M_nnnnX,
  M_immnn,M_InnX,M_immnn,M_InnX,M_nn,M_nn,M_nn,M_nn,
  M_impl,M_immnn,M_impl,M_immnn,M_nnnn,M_nnnn,M_nnnn,M_nnnn,
  M_rr,M_InnY,M_impl,M_InnY,M_nnX,M_nnX,M_nnY,M_nnY,
  M_impl,M_nnnnY,M_impl,M_nnnnY,M_nnnnX,M_nnnnX,M_nnnnY,M_nnnnY,
  M_immnn,M_InnX,M_immnn,M_InnX,M_nn,M_nn,M_nn,M_nn,
  M_impl,M_immnn,M_impl,M_immnn,M_nnnn,M_nnnn,M_nnnn,M_nnnn,
  M_rr,M_InnY,M_impl,M_InnY,M_nnX,M_nnX,M_nnY,M_nnY,
  M_impl,M_nnnnY,M_impl,M_nnnnY,M_nnnnX,M_nnnnX,M_nnnnY,M_nnnnY,
  M_immnn,M_InnX,M_immnn,M_InnX,M_nn,M_nn,M_nn,M_nn,
  M_impl,M_immnn,M_impl,M_immnn,M_nnnn,M_nnnn,M_nnnn,M_nnnn,
  M_rr,M_InnY,M_impl,M_InnY,M_nnX,M_nnX,M_nnX,M_nnX,
  M_impl,M_nnnnY,M_impl,M_nnnnY,M_nnnnX,M_nnnnX,M_nnnnX,M_nnnnX,
  M_immnn,M_InnX,M_immnn,M_InnX,M_nn,M_nn,M_nn,M_nn,
  M_impl,M_immnn,M_impl,M_immnn,M_nnnn,M_nnnn,M_nnnn,M_nnnn,
  M_rr,M_InnY,M_impl,M_InnY,M_nnX,M_nnX,M_nnX,M_nnX,
  M_impl,M_nnnnY,M_impl,M_nnnnY,M_nnnnX,M_nnnnX,M_nnnnX,M_nnnnX
};
int covered[512]={0};

int masks[512];

int bitsset(int v)
{
  int count=0;
  for(int i=1;i<(1<<20);i*=2)
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
  printf("Generating addressing_modes.vhdl\n");
  FILE *f=fopen("addressing_modes.vhdl","w");
  fprintf(f,
	  "-- DO NOT MODIFY: Auto-generated by addressingmodeequations.c\n"
	  "\n\n"
	  "library ieee;\n"
	  "use Std.TextIO.all;\n"
	  "use ieee.STD_LOGIC_1164.all;\n"
	  "use ieee.numeric_std.all;\n"
	  "use work.types.all;\n"
	  "\n"
	  "package addressing_modes is\n"
	  "\n"
	  "  type addressing_mode is record\n");	  
  for(int flag=1;flag<=MAX_FLAG;flag*=2) {
    fprintf(f,"    %s : boolean;\n",flagname(flag));
  }
  fprintf(f,
          "  end record;\n"
	  "\n"
	  "  function get_addressing_modes(opcode : opcode_type) return addressing_mode;\n"
	  "\n"
	  "end package;\n"
	  "\n"
	  "package body addressing_modes is\n"
	  "  function get_addressing_modes(opcode : opcode_type) return addressing_mode is\n"
	  "    variable mode : addressing_mode := (others => false);\n"
	  "  begin\n"
	  "    -- XXX Zero out contents of modes?\n"
	  );
  
  for(int flag=1;flag<=MAX_FLAG;flag*=2) {
    
    // Try all masks with all possible values, to see what the largest mask is
    // that we can use to cover the most unclassified remaining opcodes for this mode action.

    rule_count=0;
    
    int remaining=512;
    for(int i=0;i<512;i++) {
      covered[i]=0;
      if (!(modes[i]&flag)) {
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
		  if (!(modes[i]&flag)) { matches=-1; break; }
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
      int actual_value=(modes[i]&flag)?1:0;
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
