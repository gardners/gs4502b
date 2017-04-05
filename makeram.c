/*
  Take 256KB memory file, and create four 64KB pre-initialised VHDL memories.
  Used to initialise 128KB RAM and "ROM" memories for MEGA65. In reality, it
  is one contiguous 256KB RAM, with optional write-protecting of the ROM half.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

char *top=
  "-- A parameterized, inferable, true dual-port, dual-clock block RAM in VHDL.\n"
"\n" 
"library ieee;\n"
"use ieee.std_logic_1164.all;\n"
"use ieee.numeric_std.all;\n"
"use Std.TextIO.all;\n"
"use work.debugtools.all;\n"
"use work.visualise.all;\n"
" \n"
"entity ram0 is\n"
"generic ( entity_name : string );\n"
"port (\n"
"    -- Port A\n"
"    a_clk   : in  std_logic;\n"
"    a_wr    : in  std_logic;\n"
"    a_addr  : in  std_logic_vector(16 downto 0);\n"
"    a_din   : in  std_logic_vector(8 downto 0);\n"
"    a_dout  : out std_logic_vector(8 downto 0);\n"
"     \n"
"    -- Port B\n"
"    b_clk   : in  std_logic;\n"
"    b_wr    : in  std_logic;\n"
"    b_addr  : in  std_logic_vector(16 downto 0);\n"
"    b_din   : in  std_logic_vector(8 downto 0);\n"
"    b_dout  : out std_logic_vector(8 downto 0)\n"
");\n"
"end ram0;\n"
" \n"
"architecture rtl of ram0 is\n"
"    -- Shared memory\n"
  "  type mem_type is array ( (((256+16)*1024)/4-1) downto 0 ) of std_logic_vector(8 downto 0);    shared variable mem : mem_type  := (\n";

char *bottom=
"    others => \"000000000\");\n"
"  \n"
"begin\n"
" \n"
"-- Port A\n"
"process(a_clk)\n"
"begin\n"
"    if(a_clk'event and a_clk='1') then\n"
"        if(a_wr='1') then\n"
"            mem(to_integer(unsigned(a_addr))) := a_din;\n"
"        end if;\n"
"        a_dout <= mem(to_integer(unsigned(a_addr)));\n"
"        -- report \"Reading ram0 $\" & to_hstring(unsigned(a_addr)) & \" = $\" & to_hstring(mem(to_integer(unsigned(a_addr))));\n"
"    end if;\n"
"end process;\n"
" \n"
"-- Port B\n"
"process(b_clk)\n"
"begin\n"
"    if(b_clk'event and b_clk='1') then\n"
"        if(b_wr='1') then\n"
"            mem(to_integer(unsigned(b_addr))) := b_din;\n"
"        end if;\n"
"        b_dout <= mem(to_integer(unsigned(b_addr)));\n"
"        -- report \"Reading ram0 $\" & to_hstring(unsigned(b_addr));\n"
"    end if;\n"
"end process;\n"
" \n"
"  process (a_addr,b_addr,a_din,b_din) is\n"
"    variable ignored : boolean;\n"
"  begin\n"
"      ignored := visualise(entity_name,\"a_clk\",a_clk);\n"
"      ignored := visualise(entity_name,\"a_wr\",a_wr);\n"
"      ignored := visualise(entity_name,\"a_addr\",a_addr);\n"
"      ignored := visualise(entity_name,\"a_din\",a_din);\n"
"      ignored := visualise(entity_name,\"b_clk\",b_clk);\n"
"      ignored := visualise(entity_name,\"b_wr\",b_wr);\n"
"      ignored := visualise(entity_name,\"b_addr\",b_addr);\n"
"      ignored := visualise(entity_name,\"b_din\",b_din);\n"
"  end process;\n"
"\n"
  "end rtl;\n";

int main(int argc,char **argv)
{
  if (argc!=2) {
    fprintf(stderr,"usage: %s <inputfile>\n",argv[0]);
    exit(-1);
  }
  FILE *f=fopen(argv[1],"r");
  if (!f) {
    fprintf(stderr,"Failed to open file.\n");
    exit(-1);
  }
  
  unsigned char *data=NULL;

  data = mmap(NULL,256*1024,PROT_READ,MAP_PRIVATE,fileno(f),0);
  if (data==MAP_FAILED) {
    fprintf(stderr,"Failed to MMAP 256KB of file (fd=%d).\n",fileno(f));
    perror("why");
    exit(-1);
  }

  FILE *ram[4]={NULL,NULL,NULL,NULL};
  for(int i=0;i<4;i++) {
    char filename[1024];
    sprintf(filename,"ram%d.vhdl",i);
    ram[i]=fopen(filename,"w");
    if (!ram[i]) {
      fprintf(stderr,"Could not write to %s\n",filename);
      exit(-1);
    }

    // Output top, after substituting entity name
    char *s=strdup(top);
    char *p=s;
    while((p=strstr(p,"ram"))!=NULL) {
      p[3]='0'+i;
      p+=4;
    }
    fprintf(ram[i],"%s",s);
  }

  for(int i=0;i<256*1024;i++)
    {
      //      printf("$%05x = $%02x\n",i,data[i]);
      fprintf(ram[(i&3)]," %d => \"0%c%c%c%c%c%c%c%c\", -- $%05x = $%02x\n",
	      i>>2,
	      data[i]&128?'1':'0',
	      data[i]&64?'1':'0',
	      data[i]&32?'1':'0',
	      data[i]&16?'1':'0',
	      data[i]&8?'1':'0',
	      data[i]&4?'1':'0',
	      data[i]&2?'1':'0',
	      data[i]&1?'1':'0',
	      i,data[i]);
    }
  
  for(int i=0;i<4;i++) {
    // Output top, after substituting entity name
    char *s=strdup(bottom);
    char *p=s;
    while((p=strstr(p,"ram"))!=NULL) {
      p[3]='0'+i;
      p+=4;
    }
    fprintf(ram[i],"%s",s);
  }

  
  fclose(f);
  return 0;
}
