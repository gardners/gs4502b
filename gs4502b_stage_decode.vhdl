-- Cache line provides bottom 10 bits of 32-bit address
-- Cache .address provides bits 31 downto 10 of 32-bit address
-- Cache .pch provides bits 15 downto 8 of PC for this instruction as intended
-- to be run.  This allows the target PC of any branching instruction to be
-- pre-computed in the cache line. Both branching and non-branching PC values
-- can be fed into address translators in this stage

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.instructions.all;
use work.instruction_equations.all;
use work.address_translator.all;
use work.alu.all;

entity gs4502b_stage_decode is
  port (
    cpuclock : in std_logic;
    coreid : in integer range 0 to 2;

    current_cpu_personality : in cpu_personality;

    instruction_in : in instruction_information;
    instruction_in_valid : in boolean;
    branch8_pc : in unsigned(15 downto 0);
    branch16_pc : in unsigned(15 downto 0);
    branch8_zp_pc : in unsigned(15 downto 0);

    regs : in cpu_registers;
    
-- Input: 1-bit flag + cache line ID from execute stage to instruct us to
--        divert (whether due to branch mis-predict, RTS/RTI, interrupt or trap
--        entry/return).
    address_redirecting : in boolean;
    redirected_address : in translated_address;

-- Output: Instruction with relevant information
    instruction_out : out instruction_information;
    instruction_out_valid : out boolean;

-- Output: Vector de-reference request to prefetch stage, which will schedule
-- it in on the memory controller
    vector_fetch_address : out translated_address;
    vector_fetch_transaction_id : out unsigned(4 downto 0) := (others => '1');
-- And from prefecth we get indication when they are ready to receive a vector
-- from us.
    prefetch_ready_to_accept_vector_request : in boolean;
-- Flag to indicate if there are no pending mutations of the index/SP/B registers
-- that would prevent us calculating any indirect address (note for simplicity,
-- we use a single flag for all of these registers, instead of tracking the state
-- separately for each, which could reduce the delay in some cases).
    indirect_ready : in boolean;
    
    stall : in boolean;
    stalling : out boolean := false;
    
    -- Inputs required for address translators
    reg_mb_low : in unsigned(11 downto 0);
    reg_offset_low : in unsigned(11 downto 0);
    reg_map_low : in std_logic_vector(3 downto 0);
    reg_mb_high : in unsigned(11 downto 0);
    reg_map_high : in std_logic_vector(3 downto 0);
    reg_offset_high : in unsigned(11 downto 0);
    cpuport_value : in std_logic_vector(2 downto 0);
    cpuport_ddr : in std_logic_vector(2 downto 0);
    rom_at_8000 : in std_logic;
    rom_at_a000 : in std_logic;
    rom_at_c000 : in std_logic;
    rom_at_e000 : in std_logic;
    viciii_iomode : in std_logic_vector(1 downto 0)
    
    );
end gs4502b_stage_decode;

architecture behavioural of gs4502b_stage_decode is

  signal stalled_instruction : instruction_information;
  signal stall_buffer_occupied : boolean := false;

  signal vector_fetch_transaction_counter : unsigned(4 downto 0) := (others => '0');

  signal indirect_fast_hold : boolean := false;
  signal indirect_fast_hold_asserted : boolean := false;
  
begin

  process(cpuclock)
    variable next_line : unsigned(9 downto 0);
    variable instruction : instruction_information;
    variable branch_pc : unsigned(15 downto 0);
  begin
    if (rising_edge(cpuclock)) then
      
      if stall_buffer_occupied then
        instruction := stalled_instruction;
      else
        instruction := instruction_in;
      end if;

      report "$" & to_hstring(instruction.translated) &
        " DECODE" & integer'image(coreid)
        & " stall = " & boolean'image(stall)
        & ", instruction_in_valid = " & boolean'image(instruction_in_valid)
        & ", stall_buffer_occupied = " & boolean'image(stall_buffer_occupied)
        & ", indirect_ready = " & boolean'image(indirect_ready)
        & ", indirect_fast_hold = " & boolean'image(indirect_fast_hold);
      
      if ((stall = false) and (instruction_in_valid  or stall_buffer_occupied))
        and ((indirect_fast_hold = false)
             or (instruction.addressing_mode.indirect = false))
      then
        report "$" & to_hstring(instruction.translated) &
          " DECODE" & integer'image(coreid)
          & " : Not stalled. Decoding. reg_map_high="
          & to_string(reg_map_high)
          & ", reg_mb_high=$" & to_hstring(reg_mb_high);

        report "DECODE" & integer'image(coreid)
          & " : flags.branch_z = "
          & boolean'image(instruction.instruction_flags.branch_z);
        
        -- Decode instruction
        -- XXX Read fields from instruction bytes and work it all out
        -- For now, just lie and make every instruction an NOP

        -- Work out branch address of instruction, if relevant
        -- instruction.pc_mispredict := pc_mispredict;
        instruction.expected_translated
          := resolve_address_to_long(instruction.pc_expected,
                                     false,
                                     
                                     cpuport_value,cpuport_ddr,
                                     viciii_iomode,
                                     reg_map_low,
                                     reg_mb_low,
                                     reg_offset_low,
                                     reg_map_high,
                                     reg_mb_high,
                                     reg_offset_high,
                                     rom_at_8000,
                                     rom_at_a000,
                                     rom_at_c000,
                                     rom_at_e000);
        
        report "DECODE" & integer'image(coreid)
          & " : i.expected_translated = $"
          & to_hstring(instruction.expected_translated);

        if instruction.instruction_extra_flags.indirect_hold then
          -- Remember if this instruction will mutate a register that could impact
          -- on the calculation of an indirect operand
          indirect_fast_hold <= true;
          indirect_fast_hold_asserted <= true;
        else
          -- If not, then consider expiring indirect_fast_hold, if we know that
          -- the last potentially indirect modifying instruction has cleared.

          -- XXX Check should be done outside of stall-checked loop to avoid
          -- unnecessary wait states.

          indirect_fast_hold <= (not indirect_ready) or indirect_fast_hold_asserted;
          
          -- Also, note that we have not just seen such an instruction now, so
          -- that we can do the calculation above taking this into account
          indirect_fast_hold_asserted <= false;
        end if;
        
        -- Now work out the correct branch address from the options, by
        -- considering the addressing mode.
        -- XXX: Doesn't currently cover indirect (or indirect,X) JMP/JSR.
        if instruction.addressing_mode.rel8 then
          -- 6502-style 8-bit relative branches
          branch_pc := branch8_pc;
          instruction.pc_mispredict := branch8_pc;
          report "DECODE" & integer'image(coreid)
            & " : branch_pc = $" & to_hstring(branch_pc);
        elsif instruction.addressing_mode.rel8byte3 then
          -- 8-bit ZP conditional branch, same as 8-bit branch, but the destination
          -- address comes from the 3rd instruction byte, not the 2nd
          branch_pc := branch8_zp_pc;
          instruction.pc_mispredict := branch8_zp_pc;
          report "DECODE" & integer'image(coreid)
            & " : branch_pc = $" & to_hstring(branch_pc);
        elsif instruction.addressing_mode.rel16 then
          -- 16-bit relative branches
          branch_pc := branch16_pc;
          instruction.pc_mispredict := branch16_pc;
          report "DECODE" & integer'image(coreid)
            & " : branch_pc = $" & to_hstring(branch_pc);
        else
          -- 16-bit absolute branch address
          -- XXX - We don't have the indirect branch addresses here!
          branch_pc := instruction.bytes.arg2 & instruction.bytes.arg1;
          instruction.pc_mispredict
            := instruction.bytes.arg2 & instruction.bytes.arg1;          
          report "DECODE" & integer'image(coreid)
            & " : branch_pc = $" & to_hstring(branch_pc);
        end if;

        -- Work out address referred to by argument.  For some modes this is simple.
        -- However, the presence of the indirect modes is a REAL pain.  Worst
        -- of all are the absolute indirect modes, since we can't even use a ZP
        -- cache to deal with them. Instead we need some kind of vector lookup
        -- logic.  But lets start with the simple ones.  At least we don't have
        -- to worry about the relative addressing modes, because they are dealt
        -- with above.
        -- XXX - We have to wait until the required registers are available
        -- before we can calculate the address.  We'll add that logic in a bit
        -- later.
        instruction.argument_address(15 downto 8) := x"00";
        instruction.argument_address(7 downto 0) := instruction.bytes.arg1;
        if instruction.addressing_mode.addr16 then
          instruction.argument_address(15 downto 8) := instruction.bytes.arg2;
          if instruction.addressing_mode.postx then
            instruction.argument_address := instruction.argument_address + regs.x;
          end if;
          if instruction.addressing_mode.posty then
            instruction.argument_address := instruction.argument_address + regs.y;
          end if;
        end if;
        if instruction.addressing_mode.prex then
          -- For 6502 mode: Only increment lower byte
          -- For 4502 mode: increment both bytes (including for JMP ($nnnn,X))
          if current_cpu_personality = CPU6502 then
            -- 6502 mode: 8-bit calculation of address
            instruction.argument_address(7 downto 0) :=
              instruction.argument_address(7 downto 0) + regs.x;
          else
            -- 4502 mode: 16-bit calculation of address
            instruction.argument_address(15 downto 0) :=
              instruction.argument_address(15 downto 0) + regs.x;
          end if;
        end if;
        if instruction.addressing_mode.presp then
          instruction.argument_address := (regs.sph & regs.spl)
                                          - instruction.bytes.arg1;
        end if;
        if instruction.addressing_mode.indirect
          and prefetch_ready_to_accept_vector_request then
          -- Instruction is indirect, so we need to request reading of the vector.
          -- For simplicity, we just request the whole four bytes, and do the final
          -- resolution in the validate stage.
          -- (the validate stage will use the indirect flag to realise it must
          -- stall until it receives the vector to allow it to complete the
          -- address calculation -- that is, the loaded vector is presented to the
          -- execute stage, not to us.  This is essentially to save a cycle in
          -- the pipeline.)
          vector_fetch_address
            <= resolve_address_to_long(instruction.argument_address,
                                       false,
                                     
                                       cpuport_value,cpuport_ddr,
                                       viciii_iomode,
                                       reg_map_low,
                                       reg_mb_low,
                                       reg_offset_low,
                                       reg_map_high,
                                       reg_mb_high,
                                       reg_offset_high,
                                       rom_at_8000,
                                       rom_at_a000,
                                       rom_at_c000,
                                       rom_at_e000);
          vector_fetch_transaction_id <= vector_fetch_transaction_counter;
          instruction.vector_fetch_transaction := vector_fetch_transaction_counter;
          vector_fetch_transaction_counter <= vector_fetch_transaction_counter + 1;
          report "DECODE" & integer'image(coreid)
            & " fetching vector at $" & to_hstring(instruction.argument_address);
        end if;                
        
        instruction.mispredict_translated
          := resolve_address_to_long(branch_pc,
                                     false,
                                     
                                     cpuport_value,cpuport_ddr,
                                     viciii_iomode,
                                     reg_map_low,
                                     reg_mb_low,
                                     reg_offset_low,
                                     reg_map_high,
                                     reg_mb_high,
                                     reg_offset_high,
                                     rom_at_8000,
                                     rom_at_a000,
                                     rom_at_c000,
                                     rom_at_e000);
        
        -- CPU personality is only modified by writing to $D02F or $D640-$D67F
        -- XXX - how about a magic value for $01?
        if ((instruction.bytes.arg2 = x"D0") and (instruction.bytes.arg1 = x"2F"))
          or ((instruction.bytes.arg2 = x"D6")
              and (instruction.bytes.arg1(7 downto 6) = "01")) then
          instruction.modifies_cpu_personality := true;
        else
          instruction.modifies_cpu_personality := false;
        end if;

        if instruction_in_valid and instruction.addressing_mode.indirect
          and (not prefetch_ready_to_accept_vector_request) then
          report "$" & to_hstring(instruction.translated) &
            " DECODE" & integer'image(coreid)
            & " : Stalled waiting PREFETCH to accept our vector request) -- holding values.";
          stall_buffer_occupied <= true;
          stalled_instruction <= instruction;
          stalling <= true;
        else
          if stall_buffer_occupied then
            stall_buffer_occupied <= false;
            stalling <= true;
          else
            stalling <= false;
          end if;
        end if;
        
      else
        -- Pipeline stalled: hold existing values.
        if stall then
          report "$" & to_hstring(instruction.translated) &
            " DECODE" & integer'image(coreid)
            & " : Stalled -- holding values.";
          stalling <= true;
        else
          stalling <= false;
        end if;
        if instruction_in_valid then
          stall_buffer_occupied <= true;
        end if;
        if not stall_buffer_occupied then
          stalled_instruction <= instruction;
        end if;        
        
      end if;
      
      instruction_out <= instruction;
      instruction_out_valid <= (instruction_in_valid  or stall_buffer_occupied);
    end if;    
  end process;    
  
end behavioural;
