all:	cpu_test

SIMULATIONFILES=	cpu_test.vhdl \
			debugtools.vhdl \
			icachetypes.vhdl \
			icachebits.vhdl \
			ram0.vhdl \
			address_translator.vhdl \
			gs4502b.vhdl \
			gs4502b_instruction_prefetch.vhdl \
			gs4502b_stage_decode.vhdl \
			gs4502b_stage_validate.vhdl \
			gs4502b_stage_execute.vhdl \

icachebits.vhdl: generate_icachebits_package.csh icache_structure.txt
	./generate_icachebits_package.csh icache_structure.txt

cpu_test:	$(SIMULATIONFILES) Makefile
	ghdl -i $(SIMULATIONFILES)
	ghdl -m cpu_test

simulate:	cpu_test
	./cpu_test

transfer: cpu_test
	cd .. ;	scp -r newcpu/* 192.168.56.102:newcpu/

makeram:	makeram.c Makefile
	gcc -g -Wall -o makeram makeram.c
