GS4502B - An attempt to create a high-performance 4502 and 6502 compatible CPU
-------------------------------------------------------------------

This repository contains a work-in-progress design for a radically higher-performance 6502 compatible processor than the existing 48MHz 45GS10 processor used in the MEGA65 retro-computer.

Whereas the 45GS10 is essentially just a relatively normal 6502 core clocked at the high speed allowed by a modern FPGA, the GS4502B is a complete redesign, intended to yield both higher maximum clock-speed, as well as substantially increased instructions-per-cycle (IPC) throughput.

The three key architectural changes are:

1. Use of a relatively deep pipeline to allow increased clock speed. The increase in clock speed should be sufficient to result in no-worse instruction latency in almost all cases, and much lower instruction latency in most cases. The intention is to allow a clock speed of 192MHz, a four-fold improvement on the 45GS10.

2. The introduction of an instruction-cache (I-CACHE), to allow the processor to dispatch one instruction per cycle under normal operating conditions. Further, the I-CACHE pre-fetch logic will include the ability to fold independent consecutive instructions into a single cache entry, so that it is possible under certain conditions to obtain an IPC > 1.  However, even without instruction folding, the combination of pipeline and I-CACHE should allow an IPC approaching 1, as compared to the typical IPC of around 0.3 for a 6502 and 0.27 for the 45GS10.

3. The inclusion of powerful register and flag renaming logic, that will allow many instruction sequences that would otherwise stall the pipeline to proceed without impediment. For example, the sequence LDA $1234 / STA $2345 would be able to proceed in successive cycles, because the second instruction would be tagged to use the result of the first instruction as its operand, allowing another instruction that modifies or uses the accummulator to follow directly after. This requires that the write-back stage and memory controller have a substantial degree of intelligence, compared with the 45GS10 or a normal 6502 core.

Together, these improvements will hopefully result in a processor that is at least 10x the speed of the 45GS10, when implemented in the same FPGA device.  It is also probable that it will require less FPGA resources, due to the adoption of a more modular and scrutible design, that avoids the excessive duplication of resources that appears to occur during synthesis of the 45GS10 due to my poor programming style in that processor.  However, this is all speculation until it is actually implemented and working.

It would also have been possible to implement out-of-order execution to further increase IPC, however the logic to do so is notoriously large in area, and it is probable that it would only provide modest IPC improvements, given that we already have instruction merging and register renaming to help keep the pipeline as busy as possible.  Further improvement would require the inclusion of additional execution units, i.e., a true super-scaler design, however this would simply increase the size of the processor even more.  In any case, because the write-back stage can perform certain arithmetic operations in order to handle RMW instructions and renamed registers and flags, it already includes a low-cost form of super-scalarity in having two ALUs.

Self-modifying code
------------------

Perhaps the single greatest challenge in implementing a high-performance 6502-class processor is the wide-spread use of self-modifying code. Even the BASIC interprettor on the C64 uses it!  Worse, it is quite common to modify the very next instruction to be executed, which means that the pipeline has to be rather clever indeed to not accidentally execute the wrong version of an instruction.

Support for any and all forms of self-modifying code is quite simply a mandatory requirement for any 6502-compatible processor, and thus will be implemented in the GS4502B. The great challenge is how to do this, without harming the performance of the processor when executing non-self-modifying instructions.  Because modification and execution of instructions may be widely separated in both time and memory space, every write to memory must be checked to see if it requires updating or invalidating one or more cache lines.  Because instructions can be upto 3 bytes in length, three cache lines must be checked for every memory write that occurs.  This is, quite simply put, extremely annoying.  The GS4502B is intended to use a four-way parallel instruction cache to reduce this cost, by allowing all three offending cache lines to be read in parallel, and then also patched in parallel if required.  The fine details of how this would work are yet to be settled, and the the portion of the design that is least settled at this point in time.

