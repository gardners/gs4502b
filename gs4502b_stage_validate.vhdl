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
-- XXX - Is this happening in the execute stage now instead? It would be better
-- here, as it would save one cycle of latency.
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
-- This means that any index registers required for the addressing mode must be
-- fully resolved before the instruction can be released for execution.
-- This means LDX $1234 / LDA $2345,X will result in a stall of several cycles.
-- Not ideal, but certainly acceptable for now until we can think of ways to
-- optimise it.
-- XXX - Are we making sure that such registers are ready before asserting valid?
--
-- If there are no uncommited instructions that would affect the destination of
-- a branch, conditional branch instructions can be patched to present the correct
-- expected pc, and have their conditional flags stripped, thus reducing them
-- to unconditional branches
--
-- Another wrinkle we have to cater for is when working out if this instruction
-- has the correct value.  Normally we reference the previously released
-- instruction.  If we have checked the validity of the address of each
-- instruction before releasing it, then this will be correct, provided that no
-- branch-mispredictions occur, or equivalent events such as RTS/RTI or an
-- interrupt.  In that case we need to reference the comparison instead to the
-- requested target address.  This can be done by having the execute stage pass
-- the next desired instruction address that it wants next, together with a
-- flag to indicate when this is the value for the comparison. This will
-- naturally be delayed by one cycle in order for it to be passed back to us.
-- Thus the execute stage needs to ignore whatever it receives the cycle
-- following it asserting the processor redirect signal.

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.instructions.all;
use work.alu.all;
use work.extra_instruction_equations.all;

entity gs4502b_stage_validate is
  port (
    cpuclock : in std_logic;
    coreid : in integer;
    
-- Input: translated address of instruction in memory
    instruction_in : in instruction_information;
    instruction_in_valid : in boolean;

-- Input: What resources does this instruction require and modify?
    resources_required_in : in instruction_resources;
    resources_modified_in : in instruction_resources;
    
-- Is the instruction pipeline stalled?
    stall : in boolean;
-- What resources have just been locked by the execute stage?
    resources_freshly_locked_by_execute_stage : in instruction_resources;
    resource_lock_transaction_id_in : in transaction_id;
    resource_lock_transaction_valid_in : in boolean;
-- Current CPU personality
    current_cpu_personality : in cpu_personality;
-- Are we being redirected by the execute stage?
    address_redirecting : in boolean;
    redirected_address : in translated_address;
    redirected_pch : in unsigned(15 downto 8);

-- What can we see from the memory controller (memory access side)?
    completed_transaction : in transaction_result;
-- And is there an indirect vector that from the instruction-fetch side?
    vector_fetch_transaction_id : in unsigned(4 downto 0);
    vector_fetch_vector : in bytes4;

-- Tell decode stage when we have safely processed all pending instructions
-- that could mess with indirect address calculation.
    indirect_ready : out boolean := true;

    regs : in cpu_registers;
        
-- Output: 32-bit address source of instruction
    instruction_out : out instruction_information;
    alu_result_out : out alu_result;
    instruction_out_extra_flags : out extra_instruction_flags;
-- Output: Boolean: Is the instruction we have here ready for execution
-- (including that the pipeline is not stalled)
    instruction_valid : out boolean;
-- Instruction address is that expected by the previous instruction?
    instruction_address_is_as_expected : out boolean;
-- Output: What resources does this instruction require and modify?
    resources_required_out : out instruction_resources;
    resources_modified_out : out instruction_resources;
    
-- Output: Stall signal to tell pipeline behind us to wait
    stalling : out boolean
    
    );
end gs4502b_stage_validate;

architecture behavioural of gs4502b_stage_validate is

  -- Resources that can be modified or required by a given instruction
  signal resources_about_to_be_locked_by_execute_stage : instruction_resources := (others => false);

  -- Register and flag renaming
  signal reg_a_name : transaction_id;
  signal reg_x_name : transaction_id;
  signal reg_b_name : transaction_id;
  signal reg_y_name : transaction_id;
  signal reg_z_name : transaction_id;
  signal reg_spl_name : transaction_id;
  signal reg_sph_name : transaction_id;
  signal flag_z_name : transaction_id;
  signal flag_c_name : transaction_id;
  signal flag_v_name : transaction_id;
  signal flag_n_name : transaction_id;

  -- Remember the instruction address of the last valid instruction we
  -- announced. This greatly simplifies the check logic for the execute stage,
  -- by providing a single bit to check.  Is it therefore possible to do the
  -- complete instruction validity check? We probably can't due to branch
  -- mis-predictions alone.
  signal last_instruction_expected_address : translated_address := (others => '0');
  signal last_instruction_expected_pch : unsigned(15 downto 8) := (others => '0');
  
  -- Resources that we are still waiting to clear following memory accesses.
  -- XXX Implement logic to update this
  signal resources_what_will_still_be_outstanding_next_cycle : instruction_resources := (others => false);
  
  -- Stall buffer and stall logic
  signal stall_buffer_occupied : boolean := false;
  signal stall_out_current : boolean := false;  
  signal stalled_instruction : instruction_information;
  signal stalled_resources_required : instruction_resources;
  signal stalled_resources_modified : instruction_resources;

  signal indirect_ready_countdown : integer range 3 downto 0 := 0;
  signal indirect_ready_instruction_countdown : integer range 3 downto 0 := 0;
  signal indirect_ready_transaction_id : transaction_id;
  signal indirect_ready_transaction_pending : boolean := false;

  signal next_data_fetch_transaction_id : transaction_id := 0;
  
begin

  process(cpuclock)
    variable alu_res : alu_result;
    variable will_stall : boolean;

    -- MUX variables to choose between stall buffer and incoming instruction
    variable resources_modified : instruction_resources;
    variable resources_required : instruction_resources;
    variable instruction : instruction_information;
    variable alu_reg : unsigned(7 downto 0) := x"00";

    -- Flags that get set if we validate an instruction that will modify the
    -- inputs to an indirect address computation
    variable hold_indirect_fetch : boolean := false;
    variable hold_indirect_regop : boolean := false;
  begin
    if (rising_edge(cpuclock)) then

      -- Watch for memory transactions coming in, so that we can commit and
      -- unlock registers and flags as required.
      -- XXX Don't modify resources that will be also modified by the execute
      -- stage this cycle.
      if completed_transaction.valid = true then
        report "$" & to_hstring(last_instruction_expected_address) &
          " VALIDATE" & integer'image(coreid)
          & " : Processing completed memory transaction (tid"
          & integer'image(completed_transaction.id) & ").";

        if completed_transaction.id = reg_a_name then
          -- Must happen same cycle in execute: reg_a <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.a <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed reg A value arrived.";
        end if;
        if completed_transaction.id = reg_b_name then
          -- Must happen same cycle in execute: reg_b <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.b <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed reg B value arrived.";
        end if;
        if completed_transaction.id = reg_x_name then
          -- Must happen same cycle in execute: reg_x <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.x <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed reg X value arrived.";
        end if;
        if completed_transaction.id = reg_y_name then
          -- Must happen same cycle in execute: reg_y <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.y <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed reg Y value arrived.";
        end if;
        if completed_transaction.id = reg_z_name then
          -- Must happen same cycle in execute: reg_z <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.z <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed reg Z value arrived.";
        end if;
        if completed_transaction.id = flag_z_name then
          -- Must happen same cycle in execute: flag_z <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.flag_z <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed flag Z value arrived.";
        end if;
        if completed_transaction.id = flag_c_name then
          -- Must happen same cycle in execute: flag_c <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.flag_c <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed flag C value arrived.";
        end if;
        if completed_transaction.id = flag_n_name then
          -- Must happen same cycle in execute: flag_n <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.flag_n <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed flag N value arrived.";
        end if;
        if completed_transaction.id = flag_v_name then
          -- Must happen same cycle in execute: flag_v <= completed_transaction_value;
          resources_what_will_still_be_outstanding_next_cycle.flag_v <= false;
          report " VALIDATE" & integer'image(coreid) & "renamed flag V value arrived.";
        end if;
      end if;
      
      -- Watch for transaction announcements from execute stage so that we can
      -- rename registers and flags as required.
      -- This must come after the above, so that new locks take priority over
      -- retiring old instructions.
      if resource_lock_transaction_valid_in = true then
        report "$" & to_hstring(last_instruction_expected_address) &
          " VALIDATE" & integer'image(coreid)
          & " : Processing resource lock notification from EXECUTE.";

        if resources_freshly_locked_by_execute_stage.a then
          reg_a_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.a <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming reg A (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
        if resources_freshly_locked_by_execute_stage.b then
          reg_b_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.b <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming reg B (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
        if resources_freshly_locked_by_execute_stage.x then
          reg_x_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.x <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming reg X (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
        if resources_freshly_locked_by_execute_stage.y then
          reg_y_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.y <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming reg Y (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
        if resources_freshly_locked_by_execute_stage.z then
          reg_z_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.z <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming reg Z (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
        if resources_freshly_locked_by_execute_stage.flag_z then
          flag_z_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.flag_z <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming flag Z (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
        if resources_freshly_locked_by_execute_stage.flag_c then
          flag_c_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.flag_c <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming flag C (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
        if resources_freshly_locked_by_execute_stage.flag_v then
          flag_v_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.flag_v <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming flag V (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
        if resources_freshly_locked_by_execute_stage.flag_n then
          flag_n_name <= resource_lock_transaction_id_in;
          resources_what_will_still_be_outstanding_next_cycle.flag_n
            <= true;
          report " VALIDATE" & integer'image(coreid) & "renaming flag N (tid"
            & integer'image(resource_lock_transaction_id_in) & ".";
        end if;
      end if;

      if (instruction.translated = last_instruction_expected_address)
        and (instruction.cpu_personality = current_cpu_personality) then
        report "$" & to_hstring(last_instruction_expected_address) &
          " VALIDATE" & integer'image(coreid)
          & " : Instruction is valid.";
        
        last_instruction_expected_address <= instruction.expected_translated;
        last_instruction_expected_pch <= instruction.pc_expected(15 downto 8);
      end if;
    else
      report "$" & to_hstring(last_instruction_expected_address) &
        " VALIDATE" & integer'image(coreid)
        & " : Instruction is not valid $"
        & to_hstring(instruction.translated);
    end if;
    if address_redirecting then
      -- Remember the address we are redirecting to.
      last_instruction_expected_address <= redirected_address;
      last_instruction_expected_pch <= redirected_pch;
      report "$" & to_hstring(redirected_address) &
        " VALIDATE" & integer'image(coreid)
        & " : Redirecting PC at request of EXECUTE stage.";
    end if;
    
    -- We are stalled unless we are processing something we are reading in,
    -- and we are not being asked to stall ourselves.
    will_stall := true;
    
    if stall = false then
      -- Downstream stage is willing to accept an instruction
      report "$" & to_hstring(last_instruction_expected_address) &
        " VALIDATE" & integer'image(coreid)
        & " : not stalled";
      
      if stall_buffer_occupied then
        -- Pass instruction from our stall buffer
        instruction := stalled_instruction;
        resources_modified := stalled_resources_modified;
        resources_required := stalled_resources_required;
      else
        instruction := instruction_in;
        resources_modified := resources_modified_in;
        resources_required := resources_required_in;
        
        -- Reading from pipeline input, and we are not stalled, so we can tell
        -- the upstream pipeline stage to resume
        will_stall := false;
        stall_out_current <= false;

        report "$" & to_hstring(last_instruction_expected_address) &
          " VALIDATE" & integer'image(coreid)
          & " : not stalling upstream";

      end if;          

      report "VALIDATE" & integer'image(coreid)
        & " : i.expected_translated = $"
        & to_hstring(instruction.expected_translated);

      
      -- For unconditional jumps and branches, simply set the new PC
      if instruction.instruction_flags.do_branch
        and (not instruction.instruction_flags.do_branch_conditional) then
        instruction.pc_expected := instruction.pc_mispredict;
        instruction.expected_translated := instruction.mispredict_translated;

      -- XXX Ideally we should feed back to ourselves and previous stages
      -- the change in program flow, so that we can reduce the number of
      -- cycles incurred by taking a branch, especially a non-conditional
      -- once, where we know immediately that the branch will be taken.
      end if;

      report "VALIDATE" & integer'image(coreid)
        & " : i.expected_translated = $"
        & to_hstring(instruction.expected_translated);
      
      -- In either case above, the stall buffer becomes empty
      stall_buffer_occupied <= false;
      
      -- Pass signals through
      instruction_out <= instruction;           
      resources_modified_out <= resources_modified;
      resources_required_out <= resources_required;
      
      -- Generate extra instruction flags that are used to speed up ALU processing
      if instruction.cpu_personality = CPU6502 then
        instruction_out_extra_flags
          <= get_extra_instruction_flags("1"&std_logic_vector(instruction.bytes.opcode));
      else
        instruction_out_extra_flags
          <= get_extra_instruction_flags("0"&std_logic_vector(instruction.bytes.opcode));
      end if;
      
      if instruction.translated = last_instruction_expected_address then
        instruction_address_is_as_expected <= true;
      else
        report "$" & to_hstring(last_instruction_expected_address)
          & " VALIDATE" & integer'image(coreid)
          & " : Marking instruction as incorrect address (saw $"
          & to_hstring(instruction.translated) & ").";
        instruction_address_is_as_expected <= false;
      end if;
      
      -- Remember resources that will be potentially modified by any
      -- instructions that have not yet passed the execute stage.
      -- At the moment, the execute stage is the stage immediately
      -- following this one, so we need only remember the one instruction.
      -- XXX This memory needs to expand to multiple instructions if the
      -- pipeline becomes deeper.
      -- More specifically, we only care about resources which will be
      -- modified by this instruction following a memory access, as immediate
      -- mode, register indexing etc will all happen during the execute
      -- cycle, and so be available.  Therefore only memory load or
      -- read-modify-write instructions are a problem here.  Thus, if the
      -- instruction is a load (including stack pop) or RMW, then we need to note the
      -- resources as being delayed. In all other cases we have no
      -- delayed resources
      if instruction.instruction_flags.do_load=true then
        -- Set delayed flag for all resources modified by this
        -- instruction.
        -- XXX We can probably optimise this a bit, by not setting SPL
        -- delayed for a stack operation, for example, because the value
        -- of SP will be resolved. But we can worry about that later.
        resources_about_to_be_locked_by_execute_stage <= resources_modified;
      else
        -- Set delayed flag for all resources to false
        resources_about_to_be_locked_by_execute_stage <= (others => false);
      end if;

      -- Stall if instruction has outstanding resources, but would otherwise be
      -- fine to execute.
      if
        (not_empty(resources_required and resources_about_to_be_locked_by_execute_stage)
        or not_empty(resources_required and resources_what_will_still_be_outstanding_next_cycle)
        or not_empty(resources_required and resources_freshly_locked_by_execute_stage))
        -- Make sure instruction personality will be valid
        and (instruction.cpu_personality = current_cpu_personality) then

        report "VALIDATE" & integer'image(coreid)
          & " : stalling upstream, due to unmet renamed resource requirements to execute this instruction.";
        report "  contributors: "
          & boolean'image(not_empty(resources_required and resources_about_to_be_locked_by_execute_stage)) & ", "
          & boolean'image(not_empty(resources_required and resources_what_will_still_be_outstanding_next_cycle)) & ", "
          & boolean'image(not_empty(resources_required and resources_freshly_locked_by_execute_stage)) & ", "
        -- Make sure instruction personality will be valid
          & boolean'image(instruction.cpu_personality /= current_cpu_personality) & ", "
          & boolean'image(instruction.modifies_cpu_personality);

        will_stall := true;
        stall_out_current <= true;

        -- If we weren't already stalling the upstream, then we need to
        -- copy the current inputs to the stall buffer, and mark the stall
        -- buffer as occupied. We don't need to buffer the stalled instruction
        -- in any other circumstance, since the instruction would be invalid.
        if stall_out_current=false then
          stalled_instruction <= instruction;
          stalled_resources_modified <= resources_modified;
          stalled_resources_required <= resources_required;
          stall_buffer_occupied <= true;
        end if;     
      end if;

      
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
        --
        -- We also have to check outstanding_resources, which is the list of
        -- resources for which we are currently waiting for finalisation from
        -- the memory controller, i.e., due to resolution of renamed
        -- registers or flags.  Outstanding resources is computed by seeing
        -- what transaction information the execute stage informs us of (it
        -- is the stage that allocates transaction IDs to memory
        -- transactions, and hence does the resource re-writing.

        -- Currently locked instructions are easy to test
        not_empty(resources_required and resources_about_to_be_locked_by_execute_stage)
        or not_empty(resources_required and resources_what_will_still_be_outstanding_next_cycle)
        or not_empty(resources_required and resources_freshly_locked_by_execute_stage)
        -- Make sure instruction personality will be valid
        or (instruction.cpu_personality /= current_cpu_personality)
        or (instruction.modifies_cpu_personality)
      then
        -- Instructions resource requirements not currently met.
        -- XXX - HOLD INPUT *and* OUTPUT values
        -- What would be really nice is if we can insert bubbles in the
        -- pipeline to be closed up when we get here, so that we can avoid
        -- the need for any extra buffer registers and muxes.
        
        -- Tell downstream stage the instruction is not valid for execution.
        instruction_valid <= false;
        report "VALIDATE" & integer'image(coreid)
          & " : instruction_valid <= false due to renaming or mis-matched CPU personality.";

      else
        -- Instruction meets all requirements
        -- Release and pass forward
        -- NOTE: This only means that the instruction COULD execute.
        -- The execute stage will check if the instruction WILL in fact execute,
        -- i.e., that the instruction address and CPU personality
        -- (4502, 6502 or Hypervisor mode)
        -- XXX - We should check CPU personality here, to save the execute
        -- stage checking it, as comparing a 32-bit PC will be deep enough logic.
        -- This does mean that we might mistakenly think that some resource
        -- will be busy next cycle based on the last instruction we let through.
        -- Thus we might not dispatch instructions sometimes when we should
        -- be able to do so, but the delay will only be 1 cycle, as the
        -- actual resource locks will are read back from the execute stage.
        instruction_valid <= true;

        -- Work out if we need to hold indirect address computation until this
        -- instruction completes?
        if instruction.translated = last_instruction_expected_address then
          if instruction.instruction_extra_flags.indirect_hold and
            instruction.instruction_flags.do_load then
            hold_indirect_regop := true;
            report "VALIDATE" & integer'image(coreid)
              & " : Holding indirect address computation until this instruction completes (it will rename index registers).";
          else
            hold_indirect_regop := false;
          end if;
        end if;
      end if;
    else
      -- Pipeline stalled: hold existing values.
      report "$" & to_hstring(last_instruction_expected_address) &
        " VALIDATE" & integer'image(coreid)
        & " : Stalled";
      
      -- XXX: We should assign them so that we avoid having flip-flops.        
      instruction_valid <= false;        
    end if;

    hold_indirect_fetch := instruction.instruction_extra_flags.indirect_hold;
    
    stalling <= will_stall;
    report "$" & to_hstring(last_instruction_expected_address) &
      " VALIDATE" & integer'image(coreid)
      & " : setting stalling to " & boolean'image(will_stall)
      & ", hold_indirect_fetch = " & boolean'image(hold_indirect_fetch);

    if instruction.instruction_extra_flags.indirect_hold then -- hold_indirect_fetch then
      report "VALIDATE" & integer'image(coreid) & " : hold_indirect_fetch = true, hold tid = "& integer'image(next_data_fetch_transaction_id);
      -- We need to wait until the CPU get the result of the memory transaction
      -- associated with this instruction, and then allow 2 more cycles for it
      -- to propogate back to the decode stage.
      indirect_ready_transaction_id <= next_data_fetch_transaction_id;
      indirect_ready_transaction_pending <= true;
      indirect_ready_countdown <= 2;
      indirect_ready <= false;
    elsif hold_indirect_regop then
      -- Wait until this instruction has passed by the EXECUTE stage.
      -- EXECUTE could be stalled, however, so we can't just count cycles.
      -- Instead we need to know when the EXECUTE stage has dispatched whatever
      -- it is doing now, and also this instruction. So what we really need to
      -- count is 2 cycles where the EXECUTE stage is not stalling, and then allow
      -- 2 more cycles for it to propogate back to the decode stage.
      indirect_ready_instruction_countdown <= 2;
      indirect_ready_countdown <= 2;
      indirect_ready <= false;
      report "VALIDATE" & integer'image(coreid) & " : hold_indirect_regop = true, waiting for two cycles when execute is not stalled";
    else
      if (indirect_ready_transaction_pending and
          indirect_ready_transaction_id = completed_transaction.id)
        and (indirect_ready_instruction_countdown = 0) then
        indirect_ready_transaction_pending <= false;
        if indirect_ready_countdown >0 then
          indirect_ready_countdown <= indirect_ready_countdown - 1;
          indirect_ready <= false;
        else
          indirect_ready <= true;
        end if;
      else
        if (indirect_ready_instruction_countdown > 0) and
          (stall = false) then
          indirect_ready_instruction_countdown
            <= indirect_ready_instruction_countdown -1;
        end if;
      end if;
    end if;
    
  end process;    
  
end behavioural;
