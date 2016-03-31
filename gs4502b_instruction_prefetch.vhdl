-- 1. Every memory write could be an instance of self-modifying code (SMC). There
-- just isn't any way to know. Worse, many cases of SMC modify the very next
-- instruction to be executed, so have to be able to flush the prefetch
-- pipeline when required.  This is really just a special case of:
--
-- 2. The CPU could indicate an unexpected branch (or interrupt) at anytime, in
-- which case we need to finish what we are doing, and quickly start fetching
-- instructions from the new location.  It would be great for branches, for
-- example, to be able to have speculatively loaded the right bytes to avoid
-- latency (since we have at least 25% more memory bandwidth than required)
-- when taking unexpected branches.
--
-- 3. For JMP, JSR, BSR, BRA we can immediately redirect to the new instruction
-- address. Also for branches that we expect will be taken. This will reduce,
-- but not eliminate, the latency of executing the instructions that follow.
-- We could have a short instruction queue to help hide this.
--
-- 4. for RTS and RTI, we could also have access to the return address (which
-- we will know, since we computed the new address), and which we could use
-- immediately, provided that we know that no stack fiddling or other changes
-- have occurred that would redirect it.

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;
use work.icachebits.all;

entity gs4502b_instruction_prefetch is
  port (
    cpuclock : in std_logic;

    current_cpu_personality : in cpu_personality;
    
-- Input: 1-bit flag + cache line ID from execute stage to instruct us to
--        divert (whether due to branch mis-predict, RTS/RTI, interrupt or trap
--        entry/return).
    address_redirecting : in boolean;
    redirected_address : in translated_address;
    redirected_pch : in unsigned(15 downto 8);

    stall : in boolean;

    instruction_out : out instruction_information;

    -- Interface to 4x 64KB RAMs
    memory_address : out std_logic_vector(15 downto 0);
    memory_data0 : out std_logic_vector(8 downto 0);
    memory_data1 : out std_logic_vector(8 downto 0);
    memory_data2 : out std_logic_vector(8 downto 0);
    memory_data3 : out std_logic_vector(8 downto 0)

    
    );
end gs4502b_instruction_prefetch;

architecture behavioural of gs4502b_instruction_prefetch is
  signal instruction_address : translated_address := (others => '0');
  signal instruction_pc : unsigned(15 downto 0) := x"8100";
begin
  process (cpuclock) is
    variable instruction : instruction_information;
    variable bytes : instruction_bytes;
    variable next_pc : unsigned(15 downto 0);
  begin
    if rising_edge(cpuclock) then

      if address_redirecting = true then
        report "$" & to_hstring(instruction_address) &
          " PREFETCH : "
          & "DIVERSION requested to $" & to_hstring(redirected_address)
          & ", next_line = $"
          & to_hstring(redirected_address(9 downto 0));
        instruction_address <= redirected_address;
        instruction_pc(15 downto 8) <= redirected_pch;
        instruction_pc(7 downto 0) <= redirected_address(7 downto 0);
      else
        -- Otherwise, keep fetching from where we were.
        -- XXX Dummy incremental advance of instruction address
        instruction_address <= instruction_address + 1;
        if (instruction_address(7 downto 0) = x"FF") then
          instruction_pc(15 downto 8) <= instruction_pc(15 downto 8) + 1;
        end if;
      end if;

      -- XXX Dummy code to prepare simple dummy icache lines to feed to
      -- processor during early testing. This must be replaced with the
      -- multi-stage cache fetch pipeline that does the required memory
      -- requests, and actually prepares the instruction entry for putting into
      -- the cache.

      report "$" & to_hstring(instruction_address) & " I-FETCH";

      next_pc := to_unsigned(to_integer(instruction_address(15 downto 0)) + 1,16);
      
      instruction.does_load := false;
      instruction.does_store := false;
      instruction.modifies_cpu_personality := false;
      instruction.addressing_mode := Implied;
      instruction.instruction := Nop;
      instruction.cpu_personality := current_cpu_personality;
      instruction.bytes.opcode := x"EA";
      instruction.bytes.arg1 := x"EA";
      instruction.bytes.arg2 := x"EA";
      instruction.translated := instruction_address;
      instruction.pc := instruction_pc;
      instruction.pc_expected := next_pc;
      instruction.pc_mispredict := next_pc;
      instruction.branch_predict := false;

      instruction_out <= instruction;
    end if;
  end process;
  
end behavioural;
