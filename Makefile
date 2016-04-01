all:	cpu_test

SIMULATIONFILES=	cpu_test.vhdl \
			debugtools.vhdl \
			icachetypes.vhdl \
			icachebits.vhdl \
			ram0.vhdl ram1.vhdl ram2.vhdl ram3.vhdl \
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

transfer:
	cd .. ;	scp -r newcpu/* 192.168.56.102:newcpu/

ram0.vhdl:	makeram	mega65mem.bin
	./makeram mega65mem.bin

ram1.vhdl:	makeram	mega65mem.bin
	./makeram mega65mem.bin

ram2.vhdl:	makeram	mega65ram.bin
	./makeram mega65mem.bin

ram3.vhdl:	makeram	mega65mem.bin
	./makeram mega65mem.bin

makeram:	makeram.c Makefile
	gcc -g -Wall -o makeram makeram.c

mega65mem.bin:	c65rom.bin mega65ram.bin
	cat mega65ram.bin c65rom.bin > mega65mem.bin

mega65ram.bin:
	dd if=/dev/zero of=mega65ram.bin bs=131072 count=1
