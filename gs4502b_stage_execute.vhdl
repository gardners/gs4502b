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

entity gs4502b_stage_execute is
  port (
    cpuclock : in std_logic;
    stall : in boolean;
    reset : in std_logic;

    monitor_pc : out unsigned(15 downto 0);
    
    instruction_in : in instruction_information;
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

  -- Primary CPU state
  signal reg_pcl : unsigned(7 downto 0) := x"00";
  signal reg_pch : unsigned(7 downto 0) := x"81";
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
  signal expected_instruction_pc : unsigned(15 downto 0);

  signal wrong_instruction_count : integer range 0 to 7 := 0;
  
  -- Register and flag renaming
  signal renamed_resources : instruction_resources;
  signal res_names : resource_names;
  
begin
  process(cpuclock)
    variable regs_out : cpu_registers;
    variable renamed_out : instruction_resources;
  begin
    if (rising_edge(cpuclock)) then

      report "PC $" & to_hstring(reg_pch & reg_pcl) &
        " EXECUTE : " &
        "A:" & to_hstring(regs.a) & " " &
        "X:" & to_hstring(regs.x) & " " &
        "Y:" & to_hstring(regs.y) & " " &
        "Z:" & to_hstring(regs.z) & " " &
        "B:" & to_hstring(regs.b) & " " &
        "SP:" & to_hstring(regs.sph & regs.spl) & "  :  "
        & to_hstring(instruction_in.bytes.opcode) & " "
        & to_hstring(instruction_in.bytes.arg1) & " "
        & to_hstring(instruction_in.bytes.arg2)
        
        ;
      
      -- Make copy of registers and register renaming info for mutation.
      regs_out := regs;
      renamed_out := renamed_resources;
      
      monitor_pc(15 downto 8) <= reg_pch;
      monitor_pc(7 downto 0) <= reg_pcl;
      
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
          renamed_resources.a <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : reg_a <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.b then
          regs.b <= completed_transaction.value;
          renamed_resources.b <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : reg_b <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.x then
          regs.x <= completed_transaction.value;
          renamed_resources.x <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : reg_x <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.y then
          regs.y <= completed_transaction.value;
          renamed_resources.y <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : reg_y <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.z then
          regs.z <= completed_transaction.value;
          renamed_resources.z <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : reg_z <= $" & to_hstring(completed_transaction.value) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.flag_z then
          regs.flags.z <= completed_transaction.z;
          renamed_resources.flag_z <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : flag_z <= " & boolean'image(completed_transaction.z) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.flag_c then
          regs.flags.c <= completed_transaction.c;
          renamed_resources.flag_c <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : flag_c <= " & boolean'image(completed_transaction.c) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.flag_n then
          regs.flags.n <= completed_transaction.n;
          renamed_resources.flag_n <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : flag_n <= " & boolean'image(completed_transaction.n) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
        if completed_transaction.id = res_names.flag_v then
          regs.flags.v <= completed_transaction.v;
          renamed_resources.flag_v <= false;
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : flag_v <= " & boolean'image(completed_transaction.v) &
            " from transaction #" & integer'image(completed_transaction.id);
        end if;
      end if;

      -- Unstall pipeline by default.
      -- Instruction execution will stall it if required
      stalling <= false;
      
      if instruction_valid = false then
        -- If there is no valid instruction, then we keep expecting the same address.
        expected_instruction_address <= expected_instruction_address;
        expected_instruction_pc <= expected_instruction_pc;
        reg_pch <= instruction_in.pc(15 downto 8);
        reg_pcl <= instruction_in.pc(7 downto 0);
        report "$" & to_hstring(expected_instruction_address) &
          " EXECUTE : instruction_valid=false -- doing nothing.";        
      else
        if instruction_address_is_as_expected then
          -- Do the work of the instruction.
          wrong_instruction_count <= 0;

          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : Executing instruction.";
          -- XXX report details of instruction

          -- XXX Not yet implemented!
          -- XXX While the below plumbs in the ALU, no special instructions, or
          -- anything which touches memory (including the stack) are implemented.
          
          -- If the instruction is immediate, implied or accumulator mode
          do_alu_op(regs,
                    regs_out,
                    renamed_resources,
                    renamed_out,
                    instruction_in.instruction_flags,
                    instruction_in.bytes.arg1);
          report "EXECUTE: Doing ALU operation.";
          
          -- For now, just advance the PC to the next instruction we expect.
          expected_instruction_address <= instruction_in.expected_translated;
          expected_instruction_pc <= instruction_in.pc_expected;

          -- XXX - Almost certainly not showing the correct PCH here: there
          -- should be a PCH for both expected and mispredict cases.
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : Advancing PC to $" & to_hstring(instruction_in.pc_expected)
            & "($" & to_hstring(instruction_in.expected_translated) & ").";
          
        else
          -- Instruction address is wrong, but instruction is marked valid.
          -- XXX Need to work out the conditions under which this can occur.
          -- Is it only branch mis-predicts? If so, we can flag the mispredict
          -- when the mis-predict occurs.
          -- Can it also also happen when there is an instruction cache miss?
          -- Well, it certainly seems like that is the most likely case for how
          -- we could end up in this situation.  In which case, hopefully the
          -- instruction validate stage has already told the memory controller
          -- to fetch the correct data, so we can just do nothing here, while
          -- we wait for the data to arrive.
          report "$" & to_hstring(expected_instruction_address) &
            " EXECUTE : Ignoring validated instruction (wrong instruction address or CPU personality).";

          if wrong_instruction_count < 7 then
            wrong_instruction_count <= wrong_instruction_count + 1;
          else
            wrong_instruction_count <= 0;
            address_redirecting <= true;
            redirected_address <= expected_instruction_address;
            redirected_pch <= expected_instruction_pc(15 downto 8);
          end if;
          
        end if;
          
      end if;

      -- Commit updates to registers and renaming of resources
      regs <= regs_out;
      renamed_resources <= renamed_out;
      
      -- On reset, force PC to Hypervisor mode and entry point, and reset
      -- register values.
      if reset = '0' then

        report "$" & to_hstring(expected_instruction_address) &
          " EXECUTE : /RESET asserted ";

        current_cpu_personality <= Hypervisor;

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
        regs.b <= x"00";
        regs.x <= x"00";
        regs.y <= x"00";
        regs.z <= x"00";
        regs.sph <= x"01";
        regs.spl <= x"FF";
        
        port_value <= "111";
        port_ddr <= "111";

        -- Set address of first instruction
        address_redirecting <= true;
        redirected_address <= x"0FF88100";
        expected_instruction_address <= x"0FF88100";
        redirected_pch <= x"81";                             
        reg_pch <= x"81";
        reg_pcl <= x"00";
        
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

    
