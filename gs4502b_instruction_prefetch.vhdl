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
use work.instructions.all;
use work.addressing_modes.all;
use work.instruction_equations.all;
use work.instruction_lengths.all;
use work.alu.all;

entity gs4502b_instruction_prefetch is
  port (
    cpuclock : in std_logic;

    current_cpu_personality : in cpu_personality;
    
-- Input: 1-bit flag + destination address from execute stage to instruct us to
--        divert (whether due to branch mis-predict, RTS/RTI, interrupt or trap
--        entry/return).
    address_redirecting : in boolean;
    redirected_address : in translated_address;
    redirected_pch : in unsigned(15 downto 8);

    -- We also need to know the value of the B register, so that we can set the
    -- upper byte of the argument.  This allows us to treat ZP and ABS modes
    -- identically, and simplify some of the address calculatin logic later.
    -- This also means that setting B must flush the pipeline by asserting
    -- address_redirecting.
    regs : in cpu_registers;
    
    stall : in boolean;

    instruction_out : out instruction_information;
    instruction_out_valid : out boolean;
    branch8_pc : out unsigned(15 downto 0);
    branch16_pc : out unsigned(15 downto 0);
    branch8_zp_pc : out unsigned(15 downto 0);

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
  constant BYTE_BUFFER_WIDTH : integer := 16;
  type ilens is array (0 to BYTE_BUFFER_WIDTH) of integer;
  signal ilen_buffer : ilens;
  signal byte_buffer : unsigned((8*BYTE_BUFFER_WIDTH)-1 downto 0);
  signal bytes_ready : integer range 0 to 16 := 0;
  signal buffer_address : translated_address := (others => '0');
  signal burst_fetch : integer range 0 to (BYTE_BUFFER_WIDTH/4+1) := 0;
  signal dispatched_bytes : integer range 0 to 7 := 0;

  -- And which address are we currently looking for to append to the end of our
  -- byte buffer?
  signal desired_address : unsigned(15 downto 0) := (others => '0');

  -- Delayed signals to tell us which address and values of chip/fast RAM we are
  -- reading in a given cycle
  type prefetch_byte is record
    byte : std_logic_vector(8 downto 0);
    ilen : length_of_instruction;
  end record;
  type prefetch_vector is array ( 0 to 3 ) of prefetch_byte;
  type prefetch_buffer is record
    v : prefetch_vector;
    address : unsigned(15 downto 0);
  end record;
    
  signal fetch_buffer_1 : prefetch_buffer;
  signal fetch_buffer_2 : prefetch_buffer;
  signal fetch_buffer_3 : prefetch_buffer;
  signal fetch_buffer_now : prefetch_buffer;

  signal opcode_high_bit : std_logic := '1';

  signal skip_bytes : integer := 0;
begin
  process (cpuclock) is
    variable instruction : instruction_information;
    variable bytes : instruction_bytes;
    variable next_pc : unsigned(15 downto 0);

    variable store_offset : integer range 0 to 15 := 0;
    variable consumed_bytes : integer range 0 to 3 := 0;
    variable new_bytes_ready : integer range 0 to BYTE_BUFFER_WIDTH := 0;

    variable new_byte_buffer : unsigned((8*BYTE_BUFFER_WIDTH)-1 downto 0);
    variable new_ilen_buffer : ilens;

    variable burst_add_one : boolean := false;    
    variable burst_sub_one : boolean := false;    
  begin
    if rising_edge(cpuclock) then
      report "RISING EDGE";

      -- Provide delayed memory address and data signals, so that we know where the
      -- RAM is reading from each cycle
      fetch_buffer_now.address <= fetch_buffer_3.address;
      for i in 0 to 3 loop
        fetch_buffer_now.v(i).byte <= fetch_buffer_3.v(i).byte;
      end loop;
      -- Tag bytes with instruction lengths
      for i in 0 to 3 loop
        fetch_buffer_now.v(i).ilen
          <= instruction_length(opcode_high_bit&fetch_buffer_3.v(i).byte(7 downto 0));
      end loop;

      fetch_buffer_3.address <= fetch_buffer_2.address;
      fetch_buffer_3.v(0).byte <= memory_data0;
      fetch_buffer_3.v(1).byte <= memory_data1;
      fetch_buffer_3.v(2).byte <= memory_data2;
      fetch_buffer_3.v(3).byte <= memory_data3;
      
      fetch_buffer_2.address <= fetch_buffer_1.address;

      -- XXX When changing CPU personality, there is a 1 cycle delay before
      -- instruction lengths will be correctly calculated.  Should be fine, as
      -- we will hold CPU during personality change, anyway via
      -- address_redirecting interface, which disacrds all instruction buffer
      -- contents, and prevents it loading more until released.
      if current_cpu_personality = CPU6502 then
        opcode_high_bit <= '1';
      else
        opcode_high_bit <= '0';
      end if;


      store_offset := bytes_ready;
      consumed_bytes := 0;
      
      new_bytes_ready := bytes_ready;
      new_byte_buffer := byte_buffer;
      new_ilen_buffer := ilen_buffer;
      
      report "I-FETCH: Fetching instruction @ $" & to_hstring(instruction_address)
        & ", with " & integer'image(bytes_ready) & " bytes available.";
      
      if bytes_ready < 3 then
        instruction_out_valid <= false;
      else
        -- Work out bytes in instruction, so that we can shift down appropriately.
        -- XXX

        if skip_bytes > 0 then
          instruction_out_valid <= false;
          skip_bytes <= 0;
        else
          instruction_out_valid <= true;
        end if;
        consumed_bytes := ilen_buffer(0);
        new_bytes_ready := bytes_ready - consumed_bytes;
        
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
      new_byte_buffer(((BYTE_BUFFER_WIDTH-consumed_bytes)*8-1) downto 0)
        := byte_buffer((BYTE_BUFFER_WIDTH*8-1) downto (consumed_bytes*8));
      new_ilen_buffer(0 to (BYTE_BUFFER_WIDTH-consumed_bytes))
        := ilen_buffer(consumed_bytes to BYTE_BUFFER_WIDTH);
      -- Update where we will store, and the number of valid bytes left in
      -- the buffer.
      store_offset := bytes_ready - consumed_bytes;
      
      -- We are reading for the correct address
      report "I-FETCH: RAM READING $" & to_hstring(fetch_buffer_now.address&"00")
        &" - $" & to_hstring(fetch_buffer_now.address&"11") &
        ", stow offset " & integer'image(store_offset) & ", am hoping for $"
        & to_hstring(desired_address&"00");

      burst_sub_one := false;
      burst_add_one := false;
      
      if fetch_buffer_now.address = desired_address then
        -- But make sure we don't over flow our read queue
        report "I-FETCH: Found the bytes we were looking for to add to our buffer.";   
        if bytes_ready <= (BYTE_BUFFER_WIDTH-4) then
          report "I-FETCH: We have space, so adding to byte_buffer.";
          -- Append to the end
          for i in 0 to 3 loop
            new_byte_buffer((8*(store_offset+i)+7) downto (8*(store_offset+i)))
              := unsigned(fetch_buffer_now.v(i).byte(7 downto 0));
            new_ilen_buffer(store_offset+i) := fetch_buffer_now.v(i).ilen;
          end loop;
          -- update number of bytes available
          new_bytes_ready := bytes_ready - consumed_bytes + 4;
          report "Adding 4 to (bytes_ready-consumed_bytes) to calculate new_bytes_ready";
          -- Read next 4 bytes: this happens through next block, which has a
          -- nice new burst fetch process, to keep the buffer filled.
          desired_address <= desired_address + 1;

        end if;
      end if;

      -- Keep the instruction buffer as full as possible, without overflowing.
      if dispatched_bytes < 4 then
        dispatched_bytes <= dispatched_bytes + consumed_bytes;
      else
        dispatched_bytes <= dispatched_bytes + consumed_bytes - 4;
        burst_add_one := true;
        report "Ate 4 bytes, queuing next instruction word read.";
      end if;
      report "burst_fetch = " & integer'image(burst_fetch)
        & ", burst_add_one = " & boolean'image(burst_add_one)
        & ", burst_sub_one = " & boolean'image(burst_sub_one);
      if (burst_fetch > 0) then
        report "Requesting next instruction word (" & integer'image(burst_fetch)
          & " more to go).";
        memory_address <= std_logic_vector(fetch_buffer_1.address + 1);
        fetch_buffer_1.address <= fetch_buffer_1.address + 1;
        if (burst_add_one = false) then
          report "Decrementing burst_fetch";
          burst_fetch <= burst_fetch - 1;
        else
          report "Holding burst_fetch";
        end if;
      elsif (burst_add_one = true) then
        report "Incrementing burst_fetch";
        burst_fetch <= burst_fetch + 1;
      end if;
      -- Make sure that we don't get stuck forever waiting for bytes
      if (bytes_ready < 4) and (burst_fetch = 0) then
        burst_fetch <= (BYTE_BUFFER_WIDTH/4+1);
      end if;
      
      report "I-FETCH buffer was " & to_hstring(byte_buffer)
        &", now " & to_hstring(new_byte_buffer)
        &", with " & integer'image(new_bytes_ready)
        & " bytes ready, " & integer'image(consumed_bytes) & " bytes consumed.";

      byte_buffer <= new_byte_buffer;
      ilen_buffer <= new_ilen_buffer;
      bytes_ready <= new_bytes_ready;
      buffer_address <= buffer_address + consumed_bytes;

      report "Updated: new_bytes_ready = " & integer'image(new_bytes_ready);

      instruction_address <= instruction_address + consumed_bytes;
      instruction_pc <= instruction_pc + consumed_bytes;        
      
      if address_redirecting = true then
        report "$" & to_hstring(instruction_address) &
          " PREFETCH : "
          & "DIVERSION requested to $" & to_hstring(redirected_address)
          & ", next_line = $"
          & to_hstring(redirected_address(9 downto 0));
        instruction_address <= redirected_address(31 downto 2)&"00";
        instruction_pc(15 downto 8) <= redirected_pch;
        instruction_pc(7 downto 0) <= redirected_address(7 downto 2)&"00";

        -- Invalidate current buffer
        bytes_ready <= 0;
        -- And remember that we can fetch several words at once.
        -- Enough to fill, plus one waiting in the wings.
        burst_fetch <= (BYTE_BUFFER_WIDTH / 4) + 1;
        -- And reset the bytes eaten counter that we use to decide when to load
        -- the next word.
        dispatched_bytes <= 0;

        -- Indicate how many bytes we need to skip.
        -- To keep timing, we have to overwrite ilen_buffer(0), as muxing to
        -- pick that or skip_bytes is too slow.
        if redirected_address(1 downto 0) /= "00" then
          skip_bytes <= to_integer(redirected_address(1 downto 0));
          ilen_buffer(0) <= to_integer(redirected_address(1 downto 0));
        end if;

        -- Start reading from this address
        -- fastram/chipram from CPU side is a single 256KB RAM, composed of 4x
        -- 64KB interleaved banks. This allows us to read 4 bytes at a time.
        memory_address <= std_logic_vector(redirected_address(17 downto 2));
        fetch_buffer_1.address <= redirected_address(17 downto 2);
        desired_address <= redirected_address(17 downto 2);

      else
      -- Otherwise, keep fetching from where we were.
      end if;

      report "$" & to_hstring(instruction_address) & " I-FETCH";

      next_pc := to_unsigned(to_integer(instruction_address(15 downto 0)) + consumed_bytes,16);
      
      if current_cpu_personality = CPU6502 then
        instruction.instruction_flags
          := get_instruction_flags("1"&std_logic_vector(byte_buffer(7 downto 0)));
        instruction.addressing_mode
          := get_addressing_modes("1"&std_logic_vector(byte_buffer(7 downto 0)));
      else
        instruction.instruction_flags
          := get_instruction_flags("0"&std_logic_vector(byte_buffer(7 downto 0)));
        instruction.addressing_mode
          := get_addressing_modes("0"&std_logic_vector(byte_buffer(7 downto 0)));
      end if;      
      instruction.modifies_cpu_personality := false;
      instruction.cpu_personality := current_cpu_personality;
      instruction.bytes.opcode := byte_buffer(7 downto 0);
      instruction.bytes.arg1 := byte_buffer(15 downto 8);
      
      if bytes_ready > 2 then
        instruction.translated := instruction_address;
        instruction.bytes.arg2 := byte_buffer(23 downto 16);
      else
        instruction.translated := (others => '1');
        -- Set upper byte of address field to B register, so that we can treat
        -- ZP and ABS addressing modes equivalently. (Also gives us the option
        -- of having another CPU personality that allows (ABS),Y etc).
        instruction.bytes.arg2 := regs.b;
      end if;
      instruction.pc := instruction_pc;
      instruction.pc_expected := next_pc;
      instruction.pc_mispredict := next_pc;
      instruction.branch_predict := false;

      -- Work out possible PC values for JMP/JSR, as well as 8 and 16 bit
      -- branch options.
      branch8_pc <=
        to_unsigned(65536 + to_integer(next_pc) + to_integer(
          byte_buffer(15)&byte_buffer(15)&byte_buffer(15)&byte_buffer(15)&
          byte_buffer(15)&byte_buffer(15)&byte_buffer(15)&byte_buffer(15)&
          byte_buffer(15 downto 8)),16);
      branch16_pc <=
        to_unsigned(65536 + to_integer(next_pc) + to_integer(byte_buffer(23 downto 8)),16);
      -- For those bizarre BBR/BBS instructions, where the branch is from the
      -- 3rd byte of the instruction, not the 2nd
      branch8_zp_pc <=
        to_unsigned(65536 + to_integer(next_pc) + to_integer(
          byte_buffer(23)&byte_buffer(23)&byte_buffer(23)&byte_buffer(23)&
          byte_buffer(23)&byte_buffer(23)&byte_buffer(23)&byte_buffer(23)&
          byte_buffer(23 downto 16)),16);
      
      instruction_out <= instruction;
    end if;
  end process;

end behavioural;
