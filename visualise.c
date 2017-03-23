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
#include <stdlib.h>
#include <string.h>

struct entity {
  char *name;
  char *class;
  struct entity *children;
  struct entity *next;
};

struct entity *model=NULL;

struct signal {
  char *node;
  char *name;
  char value[1024];
  int changed;
};

#define MAX_SIGNALS 65536
int signal_count=0;
struct signal signals[MAX_SIGNALS];

int log_signal(char *node,char *signal,char *type,char *value)
{
  for(int i=0;i<signal_count;i++)
    {
      if (!strcmp(signals[i].node,node))
	if (!strcmp(signals[i].name,signal))
	  {
	    if (!strcasecmp(value,signals[i].value))
	      signals[i].changed=1;	    
	    strcpy(signals[i].value,value);
	    return 0;
	  }
    }
  if (signal_count>=MAX_SIGNALS) return -1;
  signals[signal_count].node=strdup(node);
  signals[signal_count].name=strdup(signal);
  signals[signal_count].changed=1;
  strcpy(signals[signal_count++].value,value);
  return 0;
}

int vhdl_structure_discover(int depth,char *name,char *file,struct entity **e)
{
  char filename[1024];
  snprintf(filename,1024,"%s.vhdl",file);
  FILE *f=fopen(filename,"r");
  if (!f) {
    fprintf(stderr,"ERROR: Could not open '%s'\n",filename);
    return -1;
  }

  for(int i=0;i<depth;i++) fprintf(stderr,"  ");
  fprintf(stderr," '%s' is a '%s'\n",name,file);

  struct entity *ee=calloc(sizeof(struct entity),1);
  struct entity **eee=e;
  ee->name=strdup(name);
  ee->class=strdup(file);
  while(*eee) {
    eee=&(*eee)->next;
  }
  *eee=ee;

  
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
      vhdl_structure_discover(depth+1,name,class,&ee->children);
    }
    if (sscanf(line,"%*[ ]%[^ ] : in %[^:;\( \r\n];",name,class)==2)
      {
	if (strcmp(name,"entity_name"))
	  fprintf(o,"      ignored := visualise(entity_name,\"%s\",%s);\n",
		  name,name);
      }
    if (sscanf(line,"%*[ ]signal %[^ ] : %[^:;\( \r\n];",name,class)==2)
      {
	if (strcmp(name,"entity_name"))
	  fprintf(o,"      ignored := visualise(entity_name,\"%s\",%s);\n",
		  name,name);
      }

    
    line[0]=0; fgets(line,1024,f);
  }
  fclose(f);

  fprintf(o,"    end if;\n");
  fprintf(o,"  end process;\n");
  fclose(o);
  
  return 0;
}

int emit_entity(FILE *f,int depth,char *prefix,struct entity *e)
{
  char node[1024];

  if (prefix[0])
    snprintf(node,1024,"%s.%s",prefix,e->name);
  else
    snprintf(node,1024,"%s",e->name);
  fprintf(stderr,"emit_entity('%s')\n",node);
    
  for(int i=0;i<depth;i++) fprintf(f,"  ");
  fprintf(f,"<div class=entity id=\"%s\">\n",node);
  for(int i=0;i<=depth;i++) fprintf(f,"  ");
  fprintf(f,"<div class=entitytitle>%s</div>\n",node);

  for(int i=0;i<=depth;i++) fprintf(f,"  ");
  fprintf(f,"<div class=signallist>\n");
 
  for(int i=0;i<signal_count;i++) {
    if (!strcmp(signals[i].node,node)) {
      for(int i=0;i<=depth;i++) fprintf(f,"  ");
      fprintf(f,"<div class=signal%s id=\"%s.%s\">%s = %s</div>\n",
	      signals[i].changed?"updated":"",
	      signals[i].node,signals[i].name,
	      signals[i].name,signals[i].value);
    }
  }

  for(int i=0;i<=depth;i++) fprintf(f,"  ");
  fprintf(f,"</div>\n");

  // Process children
  struct entity *c=e->children;
  if (c) {
    emit_entity(f,depth+1,node,c);
  }
  for(int i=0;i<depth;i++) fprintf(f,"  ");
  fprintf(f,"</div>\n");

  if (e->next) {
    emit_entity(f,depth,prefix,e->next);
  }
  return 0;
}

int frame_number=0;

int generate_frame(long long timestep)
{
  char filename[1024];
  snprintf(filename,1024,"html/frame%d.html",frame_number);
  FILE *f=fopen(filename,"w");
  if (!f) return -1;

  fprintf(f,"<html><head>\n");
  fprintf(f,"  <link rel=\"stylesheet\" type=\"text/css\" href=\"frame.css\"></head>\n");
  fprintf(f,"<body>\n");
  emit_entity(f,0,"",model);
  fprintf(f,"</body>\n</html>\n");
  fclose(f);

  for(int i=0;i<signal_count;i++)
    fprintf(stderr,"signal #%d : '%s' '%s' '%s'\n",
	    i,signals[i].node,signals[i].name,signals[i].value);

  // Clear change flags on signals
  for(int i=0;i<signal_count;i++) signals[i].changed=0;
  
  return 0;
}

int main(int argc,char **argv)
{
  // Build model of the system
  vhdl_structure_discover(0,"gs4502b","gs4502b",&model);

  fprintf(stderr,"Read model.\n");
  
  // Watch for VISUALISE lines in output of ghdl, and generate our visualisations
  // from that, whenever time advances.
  // The lines have a format like:
  // visualise.vhdl:75:5:@1625ns:(report note): VISUALISE:gs4502b:mem_ports_in(2):mem_port_in:false
  char line[1024],module[1024],signal[1024],type[1024],value[1024];
  long long timestamp;

  long long last_timestamp=-1;
  line[0]=0; fgets(line,1024,stdin);
  while(line[0]) {
    int r=sscanf(line,
		 "%*[^:]:%*d:%*d:@%lldns:(report note):"
		 " VISUALISE:%[^:]:%[^:]:%[^:]:%[^\r\n]",
		 &timestamp,module,signal,type,value);
    if (r==5) {
      fprintf(stderr,"%lldns:%s:%s:%s:%s\n",
	      timestamp,module,signal,type,value);
      log_signal(module,signal,type,value);
      if (timestamp!=last_timestamp&&last_timestamp>-1) {
	fprintf(stderr,"Time has advanced to %lldns\n",timestamp);
	generate_frame(last_timestamp);
      }
      last_timestamp=timestamp;
    }
    line[0]=0; fgets(line,1024,stdin);
  }
  
  return 0;
}
