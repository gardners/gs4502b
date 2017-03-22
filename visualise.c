/*
  Visualise the running CPU by interpretting the output of GHDL and drawing
  the state of the CPU at each clock tick in a series of HTML files.

  This program parses the VHDL source files for the CPU, and works out the 
  nesting of the various components.  This is used for outputting the nested <div>
  blocks in the HTML.

  When running, it looks for specially formatted report statements in the simulation
  output, and includes those in their respective divs.
 */

#include <stdio.h>

struct entity {
  char *name;
  struct entity *children;
  struct entity *next;
};

struct entity *model=NULL;

int vhdl_structure_discover(int depth,char *file,struct entity **e)
{
  char filename[1024];
  snprintf(filename,1024,"%s.vhdl",file);
  FILE *f=fopen(filename,"r");
  if (!f) {
    fprintf(stderr,"ERROR: Could not open '%s'\n",filename);
    return -1;
  }
  
  char line[1024];

  // Write template for visualiation for this VHDL file
  char templatefile[1024];
  snprintf(templatefile,1024,"vis_templates/%s.vhdl",file);
  FILE *o=fopen(templatefile,"w");
  if (!o) {
    fprintf(stderr,"ERROR: Could not open '%s'\n",templatefile);
    return -1;
  }
  fprintf(o,
	  "  process (cpuclock) is\n"
	  "    variable ignored : boolean;\n"
	  "  begin\n"
	  "    if rising_edge(cpuclock) then\n");
 
  
  line[0]=0; fgets(line,1024,f);
  while(line[0]) {
    char name[1024];
    char class[1024];
    if (sscanf(line,"%*[ ]%[^:]: entity work.%s",
	       name,class)==2) {
      for(int i=0;i<depth;i++) fprintf(stderr,"  ");
      fprintf(stderr," '%s' is a '%s'\n",name,class);
      vhdl_structure_discover(depth+1,class,e);
    }
    if (sscanf(line,"%*[ ]%[^ ] : in %[^:;\( \r\n];",name,class)==2)
      {
	fprintf(o,"      ignored := visualise(\"%s\",\"%s\",%s);\n",
		file,name,name);
      }
    if (sscanf(line,"%*[ ]signal %[^ ] : %[^:;\( \r\n];",name,class)==2)
      {
	fprintf(o,"      ignored := visualise(\"%s\",\"%s\",%s);\n",
		file,name,name);
      }

    
    line[0]=0; fgets(line,1024,f);
  }
  fclose(f);

  fprintf(o,"    end if;\n");
  fprintf(o,"  end process;\n");
  fclose(o);
  
  return 0;
}

int main(int argc,char **argv)
{
  vhdl_structure_discover(0,"gs4502b",&model);
  return 0;
}
