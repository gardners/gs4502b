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
-- Output: Cache line for the following instruction, consisting of lower
--         bits of PC value for conditional branch or not, based on branch
--         prediction flag.
--         UNLESS execute stage tells us we need to change PC abnormally
--         (eg branch mis-prediction, RTS/RTI, interrupt or trap entry/return)
    next_cache_line : out unsigned(9 downto 0);

    stall : in std_logic;
    
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
end gs4502b_stage_validate;

architecture behavioural of gs4502b_stage_validate is

  -- Resources that can be modified or required by a given instruction
  signal uncommitted_resources : instruction_resources;
  
begin

  process(cpuclock)
    variable next_line : unsigned(9 downto 0);
  begin
    if (rising_edge(cpuclock)) then
      if stall='0' then
        -- Pass signals through
        icache_src_address_out(31 downto 10) <= icache_src_address_in;
        icache_src_address_out(9 downto 0) <= icache_line_number;
        icache_bytes_out <= icache_bytes_in;
        icache_line_number_out <= icache_line_number_in;
        pch_out <= pch_in;
        branch_predict_out <= branch_predict_in;
        resources_modified_out <= resources_modified_in;
        resources_required_out <= resources_required_in;

        -- Remember resources that will be potentially modified by any uncommitted
        -- instructions. At the moment, the execute stage is the stage immediately
        -- following this one, so we need only remember the one instruction.
        uncommitted_resources <= resources_modified_in;
        
        if not_empty(resources_required_in and uncommitted_resources)
          or not_empty(resource_required_in and locked_resources) then
          -- Instructions resource requirements not currently met
        else
          -- Instruction meets all requirements
        end if;


        
        -- XXX - Validate instruction, and assert instruction_ready as appropriate.
        instruction_ready <= '0';
      else
        -- Pipeline stalled: hold existing values.
        -- XXX: We should assign them so that we avoid having flip-flops.        
        instruction_ready <= '0';        
      end if;

    end if;    
  end process;    
      
end behavioural;
