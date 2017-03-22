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
  }
  
  char line[1024];

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
	for(int i=0;i<depth;i++) fprintf(stderr,"  ");
	fprintf(stderr,"input signal '%s' of type '%s'\n",name,class);
      }
    if (sscanf(line,"%*[ ]signal %[^ ] : %[^:;\( \r\n];",name,class)==2)
      {
	for(int i=0;i<depth;i++) fprintf(stderr,"  ");
	fprintf(stderr,"internal signal '%s' of type '%s'\n",name,class);
      }

    
    line[0]=0; fgets(line,1024,f);
  }
  fclose(f);
  return 0;
}

int main(int argc,char **argv)
{
  vhdl_structure_discover(0,"gs4502b",&model);
  return 0;
}
