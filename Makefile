all:	cpu_test

SIMULATIONFILES=	cpu_test.vhdl \
			debugtools.vhdl \
			ghdl_ram72x1k.vhdl \
			gs4502b.vhdl

cpu_test:	$(SIMULATIONFILES) Makefile
	ghdl -i $(SIMULATIONFILES)
	ghdl -m cpu_test

simulate:	cpu_test
	./cpu_test

transfer: cpu_test
	cd .. ;	scp -r newcpu/* 192.168.56.102:newcpu/

