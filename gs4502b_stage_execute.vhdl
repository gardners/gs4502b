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
use work.instructions.all;
use work.alu.all;
use work.extra_instruction_equations.all;
use work.disassemble.all;

entity gs4502b_stage_execute is
  port (
    cpuclock : in std_logic;
    stall : in boolean;
    reset : in std_logic;
    coreid : in integer range 0 to 2;

    monitor_pc : out unsigned(15 downto 0);

    reg_export : out cpu_registers;
    
    instruction_in : in instruction_information;
    alu_res : in alu_result;
    instruction_in_extra_flags : in extra_instruction_flags;
    instruction_valid : in boolean;
    instruction_address_is_as_expected : in boolean;
    completed_transaction : in transaction_result;
    
    -- Are we redirecting execution?
    address_redirecting : out boolean;
    redirected_address : out translated_address;
    redirected_pch : out unsigned(15 downto 8);

    -- What resources are we locking?
    resources_locked : out instruction_resources;
    resource_lock_transaction_id_out : out transaction_id;
    resource_lock_transaction_valid_out : out boolean := false;

    reg_mb_low : out unsigned(11 downto 0);
    reg_offset_low : out unsigned(11 downto 0);
    reg_map_low : out std_logic_vector(3 downto 0);
    reg_mb_high : out unsigned(11 downto 0);
    reg_map_high : out std_logic_vector(3 downto 0);
    reg_offset_high : out unsigned(11 downto 0);
    cpuport_value : out std_logic_vector(2 downto 0);
    cpuport_ddr : out std_logic_vector(2 downto 0);
    
    -- What mode is the CPU currently in? (4502, 6502 or hypervisor)
    current_cpu_personality : out cpu_personality := CPU4502;

    -- Tell validate stage to stall?
    stalling : out boolean := false
    );
end gs4502b_stage_execute;

architecture behavioural of gs4502b_stage_execute is

  signal personality : cpu_personality := CPU4502;
  
  -- Primary CPU state
  signal reg_pcl : unsigned(7 downto 0) := x"AD";
  signal reg_pch : unsigned(7 downto 0) := x"DE";
  signal regs : cpu_registers;
  
  -- Memory mapping registers
  signal reg_map_lo : std_logic_vector(3 downto 0) := (others => '0');
  signal reg_map_hi : std_logic_vector(3 downto 0)
    := (0 => '1', 1 => '1', others => '0');
  signal reg_offset_lo : unsigned(19 downto 8) := (others => '0');
  signal reg_offset_hi : unsigned(19 downto 8) := x"800";
  signal reg_mb_lo : unsigned(11 downto 0) := (others => '0');
  signal reg_mb_hi : unsigned(11 downto 0) := x"0FF";
  signal port_ddr : std_logic_vector(2 downto 0) := "111";
  signal port_value : std_logic_vector(2 downto 0) := "111";
  
  signal expected_instruction_address : translated_address;
  
  -- Register and flag renaming
  signal renamed_resources : instruction_resources;
  signal res_names : resource_names;

  signal flushing_pipeline : boolean := false;
  constant PIPE_FLUSH_CYCLES : integer := 10;
  signal flush_cycles : integer range 0 to PIPE_FLUSH_CYCLES := 0;

begin
  process(cpuclock)
    variable regs_out : cpu_registers;
    variable renamed_out : instruction_resources;
    variable alu_res_int : alu_result;

  begin
    if (rising_edge(cpuclock)) then
      
      -- Make copy of registers and register renaming info for mutation.
      regs_out := regs;
      renamed_out := renamed_resources;
      
      monitor_pc(15 downto 8) <= reg_pch;
      monitor_pc(7 downto 0) <= reg_pcl;

      -- Export current value of registers for other stages that need them
      reg_export <= regs;
      
      -- By default CPU continues sequentially, without redirection.
      address_redirecting <= false;
      
      -- Propagate memory mapping state
      reg_mb_low <= reg_mb_lo;
      reg_offset_low <= reg_offset_lo;
      reg_map_low <= reg_map_lo;
      reg_mb_high <= reg_mb_hi;
      reg_map_high <= reg_map_hi;
      reg_offset_high <= reg_offset_hi;
      cpuport_value <= port_value;
      cpuport_ddr <= port_ddr;
      
      -- Process any completed memory transaction
      if completed_transaction.valid = true then
        if completed_transaction.id = res_names.a then
          regs.a <= completed_transaction.value;
          regs.a_dup1 <= completed_transaction.value;
          regs.a_dup2 <= completed_transaction.value;
          regs.a_dup3 <= completed_transaction.value;
          renamed_resources.a <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : reg_a <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.b then
          regs.b <= completed_transaction.value;
          renamed_resources.b <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : reg_b <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.x then
          regs.x <= completed_transaction.value;
          renamed_resources.x <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : reg_x <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.y then
          regs.y <= completed_transaction.value;
          regs.y_dup1 <= completed_transaction.value;
          renamed_resources.y <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : reg_y <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.z then
          regs.z <= completed_transaction.value;
          renamed_resources.z <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : reg_z <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.flag_z then
          regs.flags.z <= completed_transaction.z;
          renamed_resources.flag_z <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : flag_z <= " & boolean'image(completed_transaction.z) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.flag_c then
          regs.flags.c <= completed_transaction.c;
          renamed_resources.flag_c <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : flag_c <= " & boolean'image(completed_transaction.c) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.flag_n then
          regs.flags.n <= completed_transaction.n;
          renamed_resources.flag_n <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : flag_n <= " & boolean'image(completed_transaction.n) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.flag_v then
          regs.flags.v <= completed_transaction.v;
          renamed_resources.flag_v <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : flag_v <= " & boolean'image(completed_transaction.v) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
      end if;

      -- Unstall pipeline by default.
      -- Instruction execution will stall it if required
      stalling <= false;

      -- Manage pipeline flushing
      if flush_cycles > 0 then
        flush_cycles <= flush_cycles - 1;
      else
        flushing_pipeline <= false;
      end if;
      
      if (instruction_valid = false) or (flushing_pipeline = true) then
        -- If there is no valid instruction, then we keep expecting the same address.
        report "$" & to_hstring(expected_instruction_address) &
          " EXECUTE" & integer'image(coreid)
          & " : instruction_valid=" & boolean'image(instruction_valid)
          & " flushing_pipeline=" & boolean'image(flushing_pipeline)
          & " -- doing nothing.";        
      else
        if instruction_address_is_as_expected then
          -- Do the work of the instruction.

          if instruction_in.modifies_cpu_personality then
            -- CPU personality or memory map has changed: flush pipeline, and start
            -- fetching from next instruction afresh.
            address_redirecting <= true;
            report "EXECUTE" & integer'image(coreid) & " : "
              & " Flushing pipeline due to cpu personality change.";
            flushing_pipeline <= true;
            flush_cycles <= PIPE_FLUSH_CYCLES;
          end if;

          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE" & integer'image(coreid)
            & " : Executing instruction.";
          report "PC $" & to_hstring(reg_pch & reg_pcl) &
            " EXECUTE" & integer'image(coreid)
            & " Inputs : " &
            "A:" & to_hstring(regs.a) & " " &
            "X:" & to_hstring(regs.x) & " " &
            "Y:" & to_hstring(regs.y) & " " &
            "Z:" & to_hstring(regs.z) & " " &
            "B:" & to_hstring(regs.b) & " " &
            "SP:" & to_hstring(regs.sph & regs.spl)
            & " " & to_string(regs.flags) 
            & "  :  "
            & disassemble_instruction(reg_pch & reg_pcl, instruction_in.bytes,
                                      personality)
            ;


          -- XXX Not yet implemented!
          -- XXX While the below plumbs in the ALU, no special instructions, or
          -- anything which touches memory (including the stack) are implemented.
          
          -- If the instruction is immediate, implied or accumulator mode,
          -- do ALU or register op as required
          if instruction_in_extra_flags.cpx then
            alu_res_int := alu_op_cmp(regs.x,instruction_in.bytes.arg1);
          elsif instruction_in_extra_flags.cpy then
            alu_res_int := alu_op_cmp(regs.y,instruction_in.bytes.arg1);
          elsif instruction_in_extra_flags.cpz then
            alu_res_int := alu_op_cmp(regs.z,instruction_in.bytes.arg1);
          else
            -- ALU does a lot, so we need to feed it two duplicates of the accumulator
            -- so that the fanout isn't so wide that it causes significant
            -- propogation delays.
            alu_res_int := alu_op(instruction_in.instruction_flags,
                               regs.a_dup3,
                               regs.a_dup1,
                               instruction_in.bytes.arg1,
                               regs.flags.c,
                               regs.flags.d);
          end if;
          
          if instruction_in.instruction_flags.aludst_a then
            regs_out.a := alu_res_int.value;
            regs_out.a_dup1 := alu_res_int.value;
            regs_out.a_dup2 := alu_res_int.value;
            regs_out.a_dup3 := alu_res_int.value;
            renamed_out.a := false;
          end if;
          if instruction_in_extra_flags.nz_from_i2 then
            regs_out.flags.n := false;
            regs_out.flags.z := false;
            if instruction_in.bytes.arg1 = x"00" then
              regs_out.flags.z := true;
            end if;
            if instruction_in.bytes.arg1(7) = '1' then
              regs_out.flags.n := true;
            end if;
            renamed_out.flag_n := false;
            renamed_out.flag_z := false;
          end if;
          if instruction_in_extra_flags.nz_from_alu then
            regs_out.flags.n := alu_res_int.n;
            regs_out.flags.z := alu_res_int.z;
            renamed_out.flag_n := false;
            renamed_out.flag_z := false;
          end if;
          if instruction_in.instruction_flags.update_c then
            regs_out.flags.c := alu_res_int.c;
            renamed_out.flag_c := false;
          end if;
          if instruction_in.instruction_flags.update_v then
            regs_out.flags.v := alu_res_int.v;
            renamed_out.flag_v := false;
          end if;

          if instruction_in_extra_flags.reg_op then
            report "EXECUTE" & integer'image(coreid)
              & ": Doing register operation.";
            do_reg_op(regs,
                      regs_out,
                      renamed_resources,
                      renamed_out,
                      instruction_in.instruction_flags,
                      instruction_in_extra_flags,
                      instruction_in.bytes.arg1);
          end if;

          if instruction_in.instruction_flags.do_branch_conditional
            and (
              (instruction_in.instruction_flags.branch_z
               and (instruction_in.instruction_flags.branch_on_clear
                    /= regs.flags.z))
              or
              (instruction_in.instruction_flags.branch_n
               and (instruction_in.instruction_flags.branch_on_clear
                    /= regs.flags.n))
              or
              (instruction_in.instruction_flags.branch_v
               and (instruction_in.instruction_flags.branch_on_clear
                    /= regs.flags.v))
              or
              (instruction_in.instruction_flags.branch_c
               and (instruction_in.instruction_flags.branch_on_clear
                    /= regs.flags.c))
              ) then
            -- Take conditional branch
            report "EXECUTE" & integer'image(coreid)
              & ": Taking conditional branch to $"
              & to_hstring(instruction_in.pc_mispredict)
              & " = $" & to_hstring(instruction_in.mispredict_translated);
            expected_instruction_address <= instruction_in.mispredict_translated;
            reg_pch <= instruction_in.pc_mispredict(15 downto 8);
            reg_pcl <= instruction_in.pc_mispredict(7 downto 0);
            
            address_redirecting <= true;
            report "EXECUTE" & integer'image(coreid)
              & " : redirecting to $"
              & to_hstring(instruction_in.mispredict_translated);
            redirected_address <= instruction_in.mispredict_translated;
            redirected_pch <= instruction_in.pc_mispredict(15 downto 8);

            -- We need only invalidate the very next instruction.  It might
            -- well be that we are skipping forward.  If so, then there is
            -- benefit to just trying to proceed.  The instruction fetch
            -- pipeline will be flushed anyway, in case we are jumping to the
            -- middle of an instruction as the previous stream would have
            -- viewed it, but we can at least do a few more instructions
            -- before the pipeline has to be refilled.
            flushing_pipeline <= true;
            flush_cycles <= 1;

          else
            -- Branch not taken
            if instruction_in.instruction_flags.do_branch_conditional = false then
              report "EXECUTE" & integer'image(coreid)
                & ": Not taking conditional branch, branch_z = "
                & boolean'image(instruction_in.instruction_flags.branch_z);
              if instruction_in.instruction_flags.branch_z then
                report "EXECUTE: Conditional branch is on Z = "
                  & boolean'image(not instruction_in.instruction_flags.branch_on_clear);
              end if;              
              if instruction_in.instruction_flags.branch_c then
                report "EXECUTE: Conditional branch is on C = "
                  & boolean'image(not instruction_in.instruction_flags.branch_on_clear);
              end if;              
              if instruction_in.instruction_flags.branch_v then
                report "EXECUTE: Conditional branch is on V = "
                  & boolean'image(not instruction_in.instruction_flags.branch_on_clear);
              end if;              
              if instruction_in.instruction_flags.branch_n then
                report "EXECUTE: Conditional branch is on N = "
                  & boolean'image(not instruction_in.instruction_flags.branch_on_clear);
              end if;              
            end if;
            if instruction_in.instruction_flags.do_branch
              and (not instruction_in.instruction_flags.do_branch_conditional) then
              -- If we have taken a non-conditional branch, then we also need
              -- to tell the pre-fetcher what is going on.
              address_redirecting <= true;
              report "EXECUTE" & integer'image(coreid)
                & " : redirecting on unconditional branch to $"
                & to_hstring(instruction_in.expected_translated);
              redirected_address <= instruction_in.expected_translated;
              redirected_pch <= instruction_in.pc_expected(15 downto 8);
            end if;
            expected_instruction_address <= instruction_in.expected_translated;
            reg_pch <= instruction_in.pc_expected(15 downto 8);
            reg_pcl <= instruction_in.pc_expected(7 downto 0);
          end if;

          -- XXX - Almost certainly not showing the correct PCH here: there
          -- should be a PCH for both expected and mispredict cases.
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : Advancing PC to $" & to_hstring(instruction_in.pc_expected)
            & "($" & to_hstring(instruction_in.expected_translated) & ").";
          
        else
          -- Instruction address is wrong, but instruction is marked valid.
          -- XXX Need to work out the conditions under which this can occur.
          -- Is it only branch mis-predicts? If so, we can flag the mispredict
          -- when the mis-predict occurs.  This should be the case.
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : Ignoring validated instruction (wrong instruction address or CPU personality).";
          
        end if;
        
      end if;

      -- Commit updates to registers and renaming of resources
      regs <= regs_out;
      renamed_resources <= renamed_out;
      
      -- On reset, force PC to Hypervisor mode and entry point, and reset
      -- register values.
      if reset = '1' then

        report "$" & to_hstring(expected_instruction_address) &
          " EXECUTE" & integer'image(coreid)
          & " : RESET asserted ";

        current_cpu_personality <= Hypervisor;
        personality <= Hypervisor;

        -- Tell pipeline to stall while reset is held, as part of reset clamping.
        -- Pipeline stages under reset flush themselves.
        stalling <= true;

        regs.flags.e <= true;
        regs.flags.d <= false;
        regs.flags.c <= false;
        regs.flags.z <= true;
        regs.flags.n <= false;
        regs.flags.v <= false;
        regs.flags.i <= true;
        regs.a <= x"00";
        regs.a_dup1 <= x"00";
        regs.a_dup2 <= x"00";
        regs.a_dup3 <= x"00";
        regs.b <= x"00";
        regs.x <= x"00";
        regs.y <= x"00";
        regs.y_dup1 <= x"00";
        regs.z <= x"00";
        regs.sph <= x"01";
        regs.spl <= x"FF";
        
        port_value <= "111";
        port_ddr <= "111";

        -- Set address of first instruction
        address_redirecting <= true;
        case coreid is
          when 0 =>
            -- Main core.
            redirected_address <= x"0FF88100";
            expected_instruction_address <= x"0FF88100";
            redirected_pch <= x"81";
            reg_pch <= x"81";
            reg_pcl <= x"00";
            -- XXX Should start in hypervisor mode
          when 1 =>
            -- Auxiliary core 1
            redirected_address <= x"0FF88200";
            expected_instruction_address <= x"0FF88200";
            redirected_pch <= x"82";                             
            reg_pch <= x"82";
            reg_pcl <= x"00";
            -- XXX No hypervisor on auxiliary cores
          when 2 =>
            -- Auxiliary core 2
            redirected_address <= x"0FF88300";
            expected_instruction_address <= x"0FF88300";
            redirected_pch <= x"83";                             
            reg_pch <= x"83";
            reg_pcl <= x"00";
            -- XXX No hypervisor on auxiliary cores
        end case;

        
        -- Clear any register renaming state
        renamed_resources.a <= false;
        renamed_resources.b <= false;
        renamed_resources.x <= false;
        renamed_resources.y <= false;
        renamed_resources.z <= false;
        renamed_resources.flag_z <= false;
        renamed_resources.flag_c <= false;
        renamed_resources.flag_n <= false;
        renamed_resources.flag_v <= false;

        -- Set memory mapping registers to map hypervisor
        reg_map_lo <= (others => '0');
        reg_map_hi <= "0011"; -- $8000-$BFFF maps to Hypervisor memory

      end if;
      
    end if;
  end process;
end behavioural;


