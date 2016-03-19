-- This CPU pipeline stage checks if the instruction delivered from the cache
-- is in fact the instruction that we want to run, and that all required resources
-- are free.
--
-- To do this, it must remember the resources required by the most recent
-- instruction, as well as check for any undelivered results.
--
-- The intention is that memory reads into registers will result in a lock
-- being placed on the register in question.  This stage reads those locks,
-- together with looking at the resources required in this instruction, and
-- if the instruction would depend on any unavailable resource, then it will
-- hold the instruction and stall the pipeline.  Alternatively, if the CPU
-- diverts control away, or if the address from the instruction cache otherwise
-- does not match the signature of the one that has been requested, then the
-- candidate instruction is discarded, and the instruction_ready signal is not
-- asserted, thus causing the execute stage of the pipeline to idle.
--
-- While most instructions can occupy the execute state for a single cycle,
-- there are a few exceptions that we will have to handle.  The ones that
-- spring to mind are JSR, BSR, RTS and RTI, which all require placing or
-- retrieving more than one value on/off the stack, and in the case of the
-- return instruction, must block the pipeline until the new program counter
-- value is available.
--
-- Another job of this stage is to detect cache misses, i.e., when the correct
-- instruction cache line is present, but does not contain the correct
-- instruction.  In such cases we must ask the memory controller to fetch the
-- instruction in question (and which will then begin speculatively fetching
-- further instructions in that sequence as memory bandwidth allows).
--
-- The memory controller is rather intelligent in this processor, being
-- responsible for the operation of the read-modify-write instructions (thus
-- allowing the CPU to continue on to execute further instructions while they
-- are being processed, and also reducing the round-trip latency that would
-- otherwise be incurred through the pipeline to read and write the address in
-- question.  This approach is necessary because addressing modes will only be
-- resolved in the memory controller, and since many addressing modes depend on
-- the value of index registers, the target address cannot be computed until
-- the correct register values are available, which in turn requires that there
-- are no instructions ahead of it in the pipeline, as those could mutate the
-- index values.
--
-- If there are no uncommited instructions that would affect the destination of
-- a branch, conditional branch instructions can be patched to present the correct
-- expected pc, and have their conditional flags stripped, thus reducing them
-- to unconditional branches

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;

entity gs4502b_stage_validate is
  port (
    cpuclock : in std_logic;
    
-- Input: translated address of instruction in memory
    icache_src_address_in : in unsigned(31 downto 0);
-- Input: 3 instruction bytes
    icache_bytes_in : in std_logic_vector(23 downto 0);
-- Input: 8-bit PCH (PC upper byte) for this instruction
    pch_in : in unsigned(15 downto 8);
-- Input: 16-bit PC for expected case
    pc_expected : in unsigned(15 downto 0);
-- Input: 16-bit PC for branch mis-predict case
    pc_mispredict : in unsigned(15 downto 0);
-- Input: 1-bit Branch prediction flag: 1=assume take branch
    branch_predict_in : in std_logic;
-- Is the instruction pipeline stalled?
    stall : in std_logic;

-- Output: 32-bit address source of instruction
    icache_src_address_out : out unsigned(31 downto 0);
-- Output: 10-bit cache line number, so that we can detect cache misses
    icache_line_number_out : out unsigned(9 downto 0);
-- Output: 3 instruction bytes
    icache_bytes_out : out std_logic_vector(23 downto 0);
-- Output: 8-bit PCH (PC upper byte) for this instruction
    pch_out : out unsigned(15 downto 8);
-- Output: Translated PC for expected case
    pc_expected_translated : out unsigned(31 downto 0);
-- Output: 16-bit PC for branch mis-predict case
    pc_mispredict_translated : out unsigned(31 downto 0);
-- Output: Instruction decode signals that can be computed
-- Output: 1-bit Branch prediction flag: 1=assume take branch
--         (for passing to MMU if branch prediction is wrong, so that cache
--         line can be updated).
    branch_predict_out : out std_logic;
-- Output: Boolean: Is the instruction we have here ready for execution
-- (including that the pipeline is not stalled)
    instruction_ready : out std_logic
    
    );
end gs4502b_stage_validate;

architecture behavioural of gs4502b_stage_validate is

  -- Resources that can be modified or required by a given instruction
  signal uncommitted_resources : instruction_resources;

  -- Stall buffer
  signal stall_buffer_occupied : std_logic := '0';
  signal stalled_icache_src_address : unsigned(31 downto 0);
  signal stalled_icache_bytes : std_logic_vector(23 downto 0);
  signal stalled_pch : unsigned(15 downto 8);
  signal stalled_pc_expected : unsigned(15 downto 0);
  signal stalled_pc_mispredict : unsigned(15 downto 0);
  signal stalled_branch_predict : std_logic;
  
  
begin

  process(cpuclock)
    variable next_line : unsigned(9 downto 0);

    -- MUX variables to choose between stall buffer and incoming instruction
    variable icache_src_address : unsigned(31 downto 0);
    variable icache_bytes : std_logic_vector(23 downto 0);
    variable pch : unsigned(15 downto 8);
    variable pc_expected : unsigned(15 downto 0);
    variable pc_mispredict : unsigned(15 downto 0);
    variable branch_predict : std_logic;
  begin
    if (rising_edge(cpuclock)) then

      -- We are stalled unless we are processing something we are reading in,
      -- and we are not being asked to stall ourselves.
      stall_out <= '1';
      
      if stall='0' then
        -- Downstream stage is willing to accept an instruction
        
        if stall_buffer_occupied = '1' then
          -- Pass instruction from our stall buffer
          icache_src_address := stalled_icache_src_address;
          icache_line_number := stalled_icache_line_number;
          icache_bytes := stalled_icache_bytes;
          pch := stalled_pch;
          branch_predict := stalled_branch_predict;
          resources_modified := stalled_resources_modified;
          resources_required := stalled_resources_required;
        else
          icache_src_address := icache_src_address_in;
          icache_line_number := icache_line_number_in;
          icache_bytes := icache_bytes_in;
          pch := pch_in;
          branch_predict := branch_predict_in;
          resources_modified := resources_modified_in;
          resources_required := resources_required_in;
          
          -- Reading from pipeline input, and we are not stalled, so we can tell
          -- the upstream pipeline stage to resume
          stall_out <= '0';                    
        end if;

        -- In either case above, the stall buffer becomes empty
        stall_buffer_occupied <= '0';
        
        -- Pass signals through
        icache_src_address_out(31 downto 10) <= icache_src_address;
        icache_src_address_out(9 downto 0) <= icache_line_number;
        icache_bytes_out <= icache_bytes;
        icache_line_number_out <= icache_line_number;
        pch_out <= pch;
        branch_predict_out <= branch_predict;
        resources_modified_out <= resources_modified;
        resources_required_out <= resources_required;

        -- Remember resources that will be potentially modified by any uncommitted
        -- instructions. At the moment, the execute stage is the stage immediately
        -- following this one, so we need only remember the one instruction.
        -- XXX This memory needs to expand to multiple instructions if the
        -- pipeline becomes deeper.
        uncommitted_resources <= resources_modified;
        
        if
          -- Are all the resources we need here?
          --
          -- All that we care about are those resources which will not be available
          -- next cycle because they are either currently locked, or are about
          -- to be locked because the most recent instruction requires a memory
          -- access to resolve them, e.g, following a non-immediate mode ALU
          -- operation. e.g., EOR $1234 / STA $2345 would require the STA to
          -- stall until the EOR operation completes. 
          -- 
          -- (We also want to implement short-cutting register access when
          -- registers are pending being loaded from memory. This basically consists
          -- of using the memory transaction ID for a load as the source for
          -- the store operation, instead of the register, when the operation is
          -- passed to the memory controller (stores don't affect CPU flags),
          -- so we can ignore that for now.)
          --
          -- Therefore what we need to test now is whether we decided that the
          -- previous instruction will block on a memory access. If yes, then
          -- we need to hold this instruction.

          -- Currently locked instructions are easy to test
          not_empty(resource_required and locked_resources)
          or
          not_empty(resources_required and uncommitted_resources)
        then
          -- Instructions resource requirements not currently met.
          -- XXX - HOLD INPUT *and* OUTPUT values
          -- What would be really nice is if we can insert bubbles in the
          -- pipeline to be closed up when we get here, so that we can avoid
          -- the need for any extra buffer registers and muxes.
                  
          instruction_ready <= '0';
        else
          -- Instruction meets all requirements
          -- Release and pass forward
          instruction_ready <= '1';
        end if;
      else
        -- Pipeline stalled: hold existing values.
        -- XXX: We should assign them so that we avoid having flip-flops.        
        instruction_ready <= '0';        
      end if;

    end if;    
  end process;    
      
end behavioural;
