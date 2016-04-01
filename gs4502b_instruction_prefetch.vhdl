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
use work.instruction_lengths.all;

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
    memory_data0 : in std_logic_vector(8 downto 0);
    memory_data1 : in std_logic_vector(8 downto 0);
    memory_data2 : in std_logic_vector(8 downto 0);
    memory_data3 : in std_logic_vector(8 downto 0)

    
    );
end gs4502b_instruction_prefetch;

architecture behavioural of gs4502b_instruction_prefetch is
  signal instruction_address : translated_address := (others => '0');
  signal instruction_pc : unsigned(15 downto 0) := x"8100";

  -- 16 byte buffer for fetching instructions from memory
  signal byte_buffer : unsigned((8*16)-1 downto 0);
  signal bytes_ready : integer range 0 to 16 := 0;
  signal buffer_address : translated_address := (others => '0');

  -- Delayed signals to tell us which address of chip/fast RAM we are reading
  -- in a given cycle
  signal memory_address_0 : unsigned(15 downto 0) := (others => '0');
  signal memory_address_1 : unsigned(15 downto 0) := (others => '0');
  signal memory_address_2 : unsigned(15 downto 0) := (others => '0');
  -- And which address are we currently looking for to append to the end of our
  -- byte buffer?
  signal desired_address : unsigned(15 downto 0) := (others => '0');

  signal memory_data0_buf : std_logic_vector(8 downto 0);
  signal memory_data1_buf : std_logic_vector(8 downto 0);
  signal memory_data2_buf : std_logic_vector(8 downto 0);
  signal memory_data3_buf : std_logic_vector(8 downto 0);
  
begin
  process (cpuclock) is
    variable instruction : instruction_information;
    variable bytes : instruction_bytes;
    variable next_pc : unsigned(15 downto 0);

    variable store_offset : integer range 0 to 15 := 0;
    variable consumed_bytes : integer range 0 to 3 := 0;
    variable new_bytes_ready : integer range 0 to 16 := 0;

    variable new_byte_buffer : unsigned((8*16)-1 downto 0);

  begin
    if rising_edge(cpuclock) then

      -- Provide delayed memory address signal, so that we know where the RAM
      -- is reading from each cycle
      memory_address_2 <= memory_address_1;
      memory_address_1 <= memory_address_0;

      memory_data0_buf <= memory_data0;
      memory_data1_buf <= memory_data1;
      memory_data2_buf <= memory_data2;
      memory_data3_buf <= memory_data3;
      
      if buffer_address /= instruction_address then
        -- Buffer is useless, and must be reloaded
        report "I-FETCH: Flushing buffer, because we need $"
          & to_hstring(instruction_address)
          & ", but our buffer points to $" & to_hstring(buffer_address);
        
        buffer_address <= instruction_address;
        
        -- No bytes yet
        bytes_ready <= 0;

        -- fastram/chipram from CPU side is a single 256KB RAM, composed of 4x
        -- 64KB interleaved banks. This allows us to read 4 bytes at a time.
        memory_address <= std_logic_vector(instruction_address(17 downto 2));
        memory_address_0 <= instruction_address(17 downto 2);
        desired_address <= instruction_address(17 downto 2);
      else
        store_offset := bytes_ready;
        consumed_bytes := 0;
        new_bytes_ready := bytes_ready;
        report "I-FETCH: Fetching instruction @ $" & to_hstring(instruction_address);
        
        if bytes_ready >= 3 then
          -- Work out bytes in instruction, so that we can shift down appropriately.
          -- XXX

          if current_cpu_personality = CPU6502 then
            consumed_bytes := instruction_length('0'&byte_buffer(7 downto 0));
          else
            consumed_bytes := instruction_length('1'&byte_buffer(7 downto 0));
          end if;

          case consumed_bytes is
            when 1 =>
              report "I-FETCH: Instruction buffer head contains $"
                & to_hstring(byte_buffer(7 downto 0))
                & ".";
            when 2 =>
              report "I-FETCH: Instruction buffer head contains $"
                & to_hstring(byte_buffer(7 downto 0))
                & " $" & to_hstring(byte_buffer(15 downto 8))
                & ".";
            when others =>
              report "I-FETCH: Instruction buffer head contains $"
                & to_hstring(byte_buffer(7 downto 0))
                & " $" & to_hstring(byte_buffer(15 downto 8))
                & " $" & to_hstring(byte_buffer(23 downto 16))
                & ".";
          end case;          
        end if;
        
        -- Shift buffer down
        new_byte_buffer(((16-consumed_bytes)*8-1) downto 0)
          := byte_buffer((16*8-1) downto (consumed_bytes*8));
        -- Update where we will store, and the number of valid bytes left in
        -- the buffer.
        store_offset := bytes_ready - consumed_bytes;
        
        -- We are reading for the correct address
        report "I-FETCH: RAM READING $" & to_hstring(memory_address_2&"00")
          &" - $" & to_hstring(memory_address_2&"11") &
          ", stow offset " & integer'image(store_offset) & ", am hoping for $"
          & to_hstring(desired_address&"00");
        if memory_address_2 = desired_address then
          -- But make sure we don't over flow our read queue
          report "I-FETCH: Found the bytes we were looking for to add to our buffer.";
          if bytes_ready < 12 then
            report "I-FETCH: We have space, so adding to byte_buffer.";
            -- Append to the end
            new_byte_buffer((8*(store_offset+3)+7) downto (8*(store_offset+3)))
              := unsigned(memory_data3_buf(7 downto 0));
            new_byte_buffer((8*(store_offset+2)+7) downto (8*(store_offset+2)))
              := unsigned(memory_data2_buf(7 downto 0));
            new_byte_buffer((8*(store_offset+1)+7) downto (8*(store_offset+1)))
              := unsigned(memory_data1_buf(7 downto 0));
            new_byte_buffer((8*(store_offset+0)+7) downto (8*(store_offset+0)))
              := unsigned(memory_data0_buf(7 downto 0));
            new_bytes_ready := bytes_ready - consumed_bytes + 4;
            -- Read next 4 bytes
            desired_address <= desired_address + 1;
            memory_address <= std_logic_vector(desired_address + 1);
            memory_address_0 <= desired_address + 1;

          else
            -- We already have enough bytes, so we don't need to do anything.
            -- But we could use this time to perform some other memory action,
            -- possibly reading the alternate path following a branch, for
            -- example, so that we have it ready ahead of time. But that can
            -- come later. Possibly much later.
            new_bytes_ready := bytes_ready - consumed_bytes;
          end if;
          byte_buffer <= new_byte_buffer;
          report "I-FETCH buffer was " & to_hstring(byte_buffer)
            &", now " & to_hstring(new_byte_buffer)
            &", with " & integer'image(new_bytes_ready)
            & " bytes ready.";
        else
          -- Not reading from the right place yet, but we assume it is on its way,
          -- so do nothing right now, apart from wait.
        end if;
        bytes_ready <= new_bytes_ready;
        buffer_address <= buffer_address + consumed_bytes;

        -- XXX Dummy incremental advance of instruction address
        instruction_address <= instruction_address + consumed_bytes;
        instruction_pc <= instruction_pc + consumed_bytes;        
      end if;
      
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
