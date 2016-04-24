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

-- Output: Vector de-reference request to prefetch stage, which will schedule
-- it in on the memory controller
    vector_fetch_address : out unsigned(15 downto 0);
    vector_fetch_transaction_id : out unsigned(4 downto 0);
    vector_fetch_valid : out boolean := false;
    
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
  
begin

  process(cpuclock)
    variable next_line : unsigned(9 downto 0);
    variable instruction : instruction_information;
    variable branch_pc : unsigned(15 downto 0);
  begin
    if (rising_edge(cpuclock)) then

      vector_fetch_valid <= false;
      
      if stall_buffer_occupied then
        instruction := stalled_instruction;
      else
        instruction := instruction_in;
      end if;
      
      if stall = false then
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
        if instruction.addressing_mode.indirect then
          -- Instruction is indirect, so we need to request reading of the vector.
          -- For simplicity, we just request the whole four bytes, and do the final
          -- resolution in the validate stage.
          -- (the validate stage will use the indirect flag to realise it must
          -- stall until it receives the vector to allow it to complete the
          -- address calculation)
          vector_fetch_address <= instruction.argument_address;
          vector_fetch_transaction_id <= vector_fetch_transaction_counter;
          instruction.vector_fetch_transaction := vector_fetch_transaction_counter;
          vector_fetch_valid <= true;
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

        if stall_buffer_occupied then
          stall_buffer_occupied <= false;
          stalling <= true;
        else
          stalling <= false;
        end if;
        
      else
        -- Pipeline stalled: hold existing values.
        report "$" & to_hstring(instruction.translated) &
          " DECODE" & integer'image(coreid)
          & " : Stalled -- holding values.";
        stall_buffer_occupied <= true;
        stalled_instruction <= instruction;
        stalling <= true;
      end if;
      
      instruction_out <= instruction;

    end if;    
  end process;    
  
end behavioural;
