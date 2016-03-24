-- This stage receives an instruction that is clear to be executed in terms of
-- CPU resources, i.e., the instruction is not waiting on any resources that
-- are unavailable, with the exeption of resources that are already busy and
-- awaiting a memory transaction to finalise.
--
-- (This needs to be revisited, to make sure that we don't block any instruction
-- that can be progressed through to the memory controller for such processing,
-- as it is possible that we are not blocking only on resource locks that are
-- required right now.  The correct solution is probably for resources_required
-- in the validate stage to only detail those resources that are required
-- immediately, and cannot be used to finalise the instruction following the
-- completion of a memory transaction.)
--
-- The validate stage is also expected to provide us with the transaction ID to
-- use for the current instruction to mark resources with if we mutate them,
-- but with the final value for the resource being the result of a memory
-- transaction.
--
-- The only conditional testing we need to perform ourselves is whether the
-- instruction address matches the address that we expect, and that the
-- incoming instruction is marked as ready. The expected address is inherited
-- from the previously processed instruction.  This involves a 32-bit
-- comparison, which given that it is a conditional is probably too logic-deep.
-- Thus it is better for the validate stage to provide flags indicate whether
-- an instruction following another instruction has the address expected by
-- that instruction.


use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;

entity gs4502b_stage_execute is
  port (
    cpuclock : in std_logic;
    stall_in : in std_logic;
    
    instruction_address : in translated_address;
    instruction_valid : in boolean;
    instruction_address_is_as_expected : in boolean;
    completed_transaction : in transaction_result;
    instruction_bytes_in : in instruction_bytes;
    pch_in : in unsigned(15 downto 8);
    pc_expected_translated_in : translated_address;
    pc_mispredict_translated_in : translated_address;    
    
    -- Are we redirecting execution?
    address_redirecting : out boolean;
    redirected_address : out translated_address;
    redirected_pch : out unsigned(15 downto 8);

    -- What resources are we locking?
    resources_locked : out instruction_resources;
    resource_lock_transaction_id_out : out transaction_id;
    resource_lock_transaction_valid_out : out boolean := false;

    -- What mode is the CPU currently in? (4502, 6502 or hypervisor)
    current_cpu_personality : out cpu_personality := CPU4502;

    fetch_valid_out : out boolean := false;
    fetch_instruction_address_translated_out : out unsigned(31 downto 0);
    fetch_instruction_pch : out unsigned(15 downto 8);
  
    -- Tell validate stage to stall?
    stall_out : out std_logic := '0'
    );
end gs4502b_stage_execute;

architecture behavioural of gs4502b_stage_execute is

begin
  process(cpuclock)
  begin
    if (rising_edge(cpuclock)) then

      -- Tell memory controller about the next instruction to fetch
      -- By default, let it keep fetching from wherever it was upto.
      fetch_instruction_address_translated_out <= expected_instruction_address;
      fetch_instruction_pch <= pch_in;
      fetch_valid_out <= false;

      
      if instruction_valid='0' then
        -- If there is no valid instruction, then we keep expecting the same address.
        expected_instruction_address <= expected_instruction_address;

      else
        if instruction_address_is_as_expected then
          -- Do the work of the instruction.

          -- XXX

          
        else
          -- Instruction address is wrong, but instruction is marked valid.
          -- XXX Need to work out the conditions under which this can occur.
          -- Is it only branch mis-predicts? If so, we can flag the mispredict
          -- when the mis-predict occurs.
          -- Can it also also happen when there is an instruction cache miss?
          -- Well, it certainly seems like that is the most likely case for how
          -- we could end up in this situation.  So we should tell the memory
          -- controller to fetch the correct address.
          instruction_begin_fetching_out <= true;
        end if;
          
      end if;
    end if;
  end process;
end behavioural;

    
