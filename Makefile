all:	cpu_test

SIMULATIONFILES=	cpu_test.vhdl \
			debugtools.vhdl \
			alu.vhdl \
			instructions.vhdl \
			instruction_lengths.vhdl \
			addressing_modes.vhdl \
			instruction_equations.vhdl \
			extra_instruction_equations.vhdl \
			ram0.vhdl ram1.vhdl ram2.vhdl ram3.vhdl \
			address_translator.vhdl \
			gs4502b.vhdl \
			memory_controller.vhdl \
			gs4502b_core.vhdl \
			gs4502b_instruction_prefetch.vhdl \
			gs4502b_stage_decode.vhdl \
			gs4502b_stage_validate.vhdl \
			gs4502b_stage_execute.vhdl \
			disassemble.vhdl \
			visualise.vhdl

GHDL=/usr/local/ghdl-0.34/bin/ghdl

clean:
	rm *.o *.cf cpu_test makeram

%.bin: %.a65
	Ophis/bin/ophis -4 $<

cpu_test:	$(SIMULATIONFILES) Makefile
	$(GHDL) -i $(SIMULATIONFILES)
	$(GHDL) -m cpu_test

visualise:	visualise.c Makefile
	$(CC) -g -Wall $(COPT) -o visualise visualise.c $(LOPT)

simulate:	cpu_test
	./cpu_test || $(GHDL) -r cpu_test

simulateandlog:	cpu_test
	./cpu_test --vcd=newcpu.vcd || $(GHDL) --vcd=newcpu.vcd -r cpu_test

transfer:
	cd .. ;	scp newcpu/* 192.168.56.102:newcpu/ ; scp -r newcpu/newcpu/* 192.168.56.102:newcpu/newcpu/

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

c65rom.bin:
	dd if=/dev/zero of=c65rom.bin bs=131072 count=1

instrlenequations:	instrlenequations.c
	gcc -g -Wall -o instrlenequations instrlenequations.c

instruction_lengths.vhdl:	instrlenequations
	./instrlenequations

addressingmodeequations:	addressingmodeequations.c
	gcc -g -Wall -o addressingmodeequations addressingmodeequations.c

addressing_modes.vhdl:	addressingmodeequations
	./addressingmodeequations

instructionequations:	instructionequations.c instruction_flags.h Makefile
	gcc -O3 -g -Wall -o instructionequations instructionequations.c

extrainstructionflags:	extrainstructionflags.c extra_instruction_flags.h Makefile
	gcc -O3 -g -Wall -o extrainstructionflags extrainstructionflags.c

instruction_flags.h:	Makefile instructionequations.c extractflags
	./extractflags

extra_instruction_flags.h:	Makefile extrainstructionflags.c extractextraflags
	./extractextraflags

instruction_equations.vhdl:	instructionequations
	./instructionequations

extra_instruction_equations.vhdl:	extrainstructionflags
	./extrainstructionflags

Ophis/bin/ophis:
	git submodule init ; git submodule update

ghdl-bin/bin/ghdl:
	git submodule init ; git submodule update
	cd ghdl && ./configure --prefix=../ghdl-bin && make

