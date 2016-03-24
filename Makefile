all:	cpu_test

SIMULATIONFILES=	cpu_test.vhdl \
			debugtools.vhdl \
			icachetypes.vhdl \
			ghdl_ram72x1k.vhdl \
			address_translator.vhdl \
			gs4502b.vhdl \
			gs4502b_stage_decode.vhdl \
			gs4502b_stage_validate.vhdl \
			gs4502b_stage_execute.vhdl \

cpu_test:	$(SIMULATIONFILES) Makefile
	ghdl -i $(SIMULATIONFILES)
	ghdl -m cpu_test

simulate:	cpu_test
	./cpu_test

transfer: cpu_test
	cd .. ;	scp -r newcpu/* 192.168.56.102:newcpu/

