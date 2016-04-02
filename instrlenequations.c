#include <stdio.h>
#include <stdlib.h>

#define M_impl 1
#define M_InnX 2
#define M_nn 2
#define M_A 1
#define M_rr 2
#define M_InnY 2
#define M_InnZ 2
#define M_rrrr 3
#define M_InnnnX 3
#define M_nnnn 3
#define M_nnnnX 3
#define M_nnnnY 3
#define M_immnnnn 3
#define M_nnrr 3
#define M_immnn 2
#define M_nnX 2
#define M_nnY 2
#define M_Innnn 3
#define M_InnSPY 2

int byte_counts[512]={
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

int masks[511];

int bitsset(int v)
{
  int count=0;
  for(int i=1;i<512;i*=2)
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
  int len;
};

#define MAX_RULES 512
int rule_count=0;
struct rule rules[MAX_RULES];

int main()
{
  // Work out all possible masks
  for(int i=0;i<511;i++) masks[i]=i+1;
  qsort(masks,511,sizeof(int),compare_bitcounts);
  
  // Try all masks with all possible values, to see what the largest mask is
  // that we can use to cover the most unclassified remaining opcodes.

  int remaining=512;
  for(int i=0;i<512;i++) {
    if (byte_counts[i]==3) {
      covered[i]=1; remaining--;
    }
  }
  
  while(remaining>0) {  
    int best_matches=-1;
    int best_m=-1, best_v=-1;
    int best_length=-1;
    for(int m=0;m<511;m++)
      {
	int last_mv=-1;
	for(int v=0;v<512;v++) {
	  int mv=m&v;
	  if (mv!=last_mv) {
	    int matches=0;
	    int length=-1;
	    for(int i=0;i<512;i++) {
	      if ((i&m)==v) {
		if (length==-1) length=byte_counts[i];
		else if (length!=byte_counts[i]) { matches=-1; break; }
		if (!covered[i]) matches++;
	      }
	    }
	    if (length!=3)
	      if (matches>best_matches) {
		best_m=m; best_v=v;
		best_matches=matches;
		best_length=length;
	      }
	    last_mv=mv;
	  }
	}
      }
    for(int i=0;i<512;i++) if ((i&best_m)==best_v) { if (!covered[i]) remaining--; covered[i]=1; }
    printf("Rule #%d: OPCODE & $%03x = $%03x -> length is %d (covers %d/%d remaining instructions).\n",
	   rule_count,best_m,best_v,best_length,best_matches,remaining);
    rules[rule_count].m=best_m;
    rules[rule_count].v=best_v;
    rules[rule_count++].len=best_length;
  }

  // Now verify rules
  printf("Verifying that %d rules correctly classify lengths of all instructions.\n",
	 rule_count);
  for(int i=0;i<512;i++) {
    int actual_length=byte_counts[i];
    int predicted_length=-1;
    for(int r=0;r<rule_count;r++)
      {
	if ((i&rules[r].m)==rules[r].v) {
	  if (predicted_length==-1) predicted_length=rules[r].len;
	  else if (predicted_length!=rules[r].len) {
	    printf("Rules produce conflicting values for $%03x\n",i);
	    exit(-1);
	  }
	}
      }
    if (predicted_length==-1) predicted_length=3;
    if (predicted_length!=actual_length) {
      printf("Rules produced wrong value for $%03x: expected %d, but saw %d\n",
	     i,actual_length,predicted_length);
      exit(-1);
    }
  }

  // Okay, we now have a set of rules that we can apply.
  // Generate VHDL file
  printf("Generating instruction_lengths.vhdl\n");
  FILE *f=fopen("instruction_lengths.vhdl","w");
  fprintf(f,
	  "-- DO NOT MODIFY: Auto-generated by instrlenequations.c\n"
	  "\n\n"
	  "library ieee;\n"
	  "use Std.TextIO.all;\n"
	  "use ieee.STD_LOGIC_1164.all;\n"
	  "use ieee.numeric_std.all;\n"
	  "\n"
	  "package instruction_lengths is\n"
	  "\n"
	  "function instruction_length(opcode : std_logic_vector(8 downto 0)) return integer;\n"
	  "\n"
	  "end package;\n"
	  "\n"
	  "package body instruction_lengths is\n"
	  "  function instruction_length(opcode : std_logic_vector(8 downto 0)) return integer is\n"
	  "  begin\n"
	  );
  for(int l=1;l<3;l++)
    for(int r=0;r<rule_count;r++) {
      if (rules[r].len==l)
	fprintf(f,"    if (opcode and \"%c%c%c%c%c%c%c%c%c\") = \"%c%c%c%c%c%c%c%c%c\" then return %d; end if;\n",
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
		rules[r].len);
    }
  fprintf(f,
	  "    return 3;\n"
	  "  end function;\n"
	  "end package body;\n");
  fclose(f);
  return 0;
  
}
