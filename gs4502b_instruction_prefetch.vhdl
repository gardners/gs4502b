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
    reset : in std_logic;
    coreid : in integer range 0 to 2;
    primary_core_boost : in boolean;

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

    -- We also need to know when we are being asked to provide an indirect
    -- vector for one of the indirect addressing modes
    vector_fetch_address : in unsigned(15 downto 0);
    vector_fetch_transaction_id : in unsigned(4 downto 0);
    vector_fetch_valid : in boolean := false;
    
    instruction_out : out instruction_information;
    instruction_out_valid : out boolean;
    branch8_pc : out unsigned(15 downto 0);
    branch16_pc : out unsigned(15 downto 0);
    branch8_zp_pc : out unsigned(15 downto 0);

    -- Interface to memory
    fetch_port_write : out fetch_port_in;
    fetch_port_read : in fetch_port_out
    
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
  signal fetch_address : translated_address := (others => '0');
  signal burst_fetch : integer range 0 to (BYTE_BUFFER_WIDTH/4+1) := 0;
  signal dispatched_bytes : integer range 0 to 7 := 0;

  -- And which address are we currently looking for to append to the end of our
  -- byte buffer?
  signal desired_address : translated_address := (others => '0');
  signal ifetch_transaction_counter : unsigned(4 downto 0) := (others => '0');
  signal ifetch_expected_transaction_counter : unsigned(4 downto 0) := (others => '0');

  -- Delayed signals to tell us which address and values of chip/fast RAM we are
  -- reading in a given cycle
  type prefetch_byte is record
    byte : std_logic_vector(8 downto 0);
    ilen : length_of_instruction;
  end record;
  type prefetch_vector is array ( 0 to 3 ) of prefetch_byte;
  type prefetch_buffer is record
    v : prefetch_vector;
    address : translated_address;
    user_flags : std_logic_vector(7 downto 0);
  end record;
  
  signal fetch_buffer_1 : prefetch_buffer;
  signal fetch_buffer_now : prefetch_buffer;
  signal fetch_buffer_now_valid : boolean := false;

  signal opcode_high_bit : std_logic := '1';

  signal fetched_last_cycle : boolean := false;

  signal end_of_trace : boolean := false;

  signal space_for_bytes : boolean := true;
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

    variable fetch_port_used : boolean := false;
  begin
    if rising_edge(cpuclock) then

      -- Only mark fetch port in use when we push something new to it.
      fetch_port_used := false;
      
      -- Handle memory read pipeline.     
      fetch_buffer_now.address <= fetch_buffer_1.address;
      fetch_buffer_now.user_flags <= fetch_buffer_1.user_flags;
      for i in 0 to 3 loop
        fetch_buffer_now.v(i).byte <= fetch_buffer_1.v(i).byte;
      end loop;
      -- Tag bytes with instruction lengths
      for i in 0 to 3 loop
        fetch_buffer_now.v(i).ilen
          <= instruction_lengths
          .instruction_length(opcode_high_bit&fetch_buffer_1.v(i).byte(7 downto 0));
      end loop;

      -- Work out if this is the thing we will want next cycle?
      -- Bit 5 of user_flags is used to indicate if it is an instruction fetch
      -- or indirect address resolution.  0 = instruction fetch, which is what
      -- we care about here.
      if (std_logic_vector(to_unsigned(coreid+1,2))
          = fetch_buffer_1.user_flags(7 downto 6))
        and (address_redirecting = false)
        and ((fetch_buffer_now_valid 
              and fetch_buffer_1.user_flags(5) = '0'
              and fetch_buffer_1.user_flags(4 downto 0)
              = std_logic_vector(ifetch_expected_transaction_counter + 1))
             or ((fetch_buffer_now_valid = false)
                 and fetch_buffer_1.user_flags(5) = '0'
                 and fetch_buffer_1.user_flags(4 downto 0)
                 = std_logic_vector(ifetch_expected_transaction_counter))) then
        fetch_buffer_now_valid <= true;
      else
        fetch_buffer_now_valid <= false;
      end if;


      fetch_buffer_1.address <= fetch_port_read.translated;
      fetch_buffer_1.user_flags <= fetch_port_read.user_flags;
      for i in 0 to 3 loop
        fetch_buffer_1.v(i).byte <= fetch_port_read.bytes(i);
      end loop;
      
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
      
      report "I-FETCH" & integer'image(coreid)
        & " : Fetching instruction @ $" & to_hstring(instruction_address)
        & ", with " & integer'image(bytes_ready) & " bytes available.";
      
      if bytes_ready < 3 then
        instruction_out_valid <= false;
      else
        -- Work out bytes in instruction, so that we can shift down appropriately.

        instruction_out_valid <= true;
        consumed_bytes := ilen_buffer(0);
        new_bytes_ready := bytes_ready - consumed_bytes;
        
        case consumed_bytes is
          when 1 =>
            report "I-FETCH" & integer'image(coreid)
              & " : Instruction buffer head contains $"
              & to_hstring(byte_buffer(7 downto 0))
              & ".";
          when 2 =>
            report "I-FETCH" & integer'image(coreid)
              & " : Instruction buffer head contains $"
              & to_hstring(byte_buffer(7 downto 0))
              & " $" & to_hstring(byte_buffer(15 downto 8))
              & ".";
          when others =>
            report "I-FETCH" & integer'image(coreid)
              & " : Instruction buffer head contains $"
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
      report "I-FETCH" & integer'image(coreid)
        & " : RAM READING $" & to_hstring(fetch_buffer_now.address)
        &" - $" & to_hstring(fetch_buffer_now.address+3) &
        ", stow offset " & integer'image(store_offset) & ", am hoping for $"
        & to_hstring(desired_address)
        & " address_redirecting=" & boolean'image(address_redirecting)
        & ", reset=" & std_logic'image(reset);

      burst_sub_one := false;
      burst_add_one := false;

      if (address_redirecting = false) and (reset = '0') then
        report "I-FETCH" & integer'image(coreid)
          & " : Waiting for Tid=$" & to_hstring(to_unsigned(coreid+1,2)&"0"
                                                &ifetch_expected_transaction_counter)
          & ", just saw $" & to_hstring(fetch_buffer_now.user_flags);
        if fetch_buffer_now_valid then
          -- But make sure we don't over flow our read queue
          report "I-FETCH: Found the bytes we were looking for to add to our buffer.";   
          if space_for_bytes then
            report "I-FETCH" & integer'image(coreid)
              & " : We have space, so adding to byte_buffer.";
            -- Append to the end
            for i in 0 to 3 loop
              new_byte_buffer((8*(store_offset+i)+7) downto (8*(store_offset+i)))
                := unsigned(fetch_buffer_now.v(i).byte(7 downto 0));
              new_ilen_buffer(store_offset+i) := fetch_buffer_now.v(i).ilen;
            end loop;
            -- update number of bytes available
            new_bytes_ready := bytes_ready - consumed_bytes + 4;
            report "I-FETCH" & integer'image(coreid)
              & " : Adding 4 to (bytes_ready-consumed_bytes) to calculate new_bytes_ready";
            -- Read next 4 bytes: this happens through next block, which has a
            -- nice new burst fetch process, to keep the buffer filled.
            desired_address <= desired_address + 4;
            ifetch_expected_transaction_counter
              <= ifetch_expected_transaction_counter + 1;
            report "I-FETCH" & integer'image(coreid)
              & " : desired_address <= $" & to_hstring(desired_address + 4);
          end if;
        else
          report "I-FETCH" & integer'image(coreid)
            & " : Wrong bytes presented : desired_address = $" & to_hstring(desired_address)
            & ", fetch_buffer_now.address=$" & to_hstring(fetch_buffer_now.address);
        end if;

        -- Keep the instruction buffer as full as possible, without overflowing.
        if dispatched_bytes < 4 then
          dispatched_bytes <= dispatched_bytes + consumed_bytes;
        else
          dispatched_bytes <= dispatched_bytes + consumed_bytes - 4;
          burst_add_one := true;
          report "I-FETCH" & integer'image(coreid)
            & " : Ate 4 bytes, queuing next instruction word read.";
        end if;
        report "I-FETCH" & integer'image(coreid)
          & " : burst_fetch = " & integer'image(burst_fetch)
          & ", burst_add_one = " & boolean'image(burst_add_one)
          & ", burst_sub_one = " & boolean'image(burst_sub_one);
        if (burst_fetch > 0) and (not end_of_trace)
           and (not address_redirecting) then
          report "I-FETCH" & integer'image(coreid)
            & " : Requesting next instruction word (" & integer'image(burst_fetch)
            & " more to go, ready="
            & boolean'image(fetch_port_read.ready) &
            ").";
          if fetch_port_read.ready or (primary_core_boost and (coreid=0)) then
            report "FETCH" & integer'image(coreid)
              & " port ready: Asking for $"
              & to_hstring(fetch_address + 4)
              & " as Tid $" & to_hstring(to_unsigned(coreid+1,2)&"0"&
                                         ifetch_transaction_counter);
            fetch_port_write.valid <= true;
            fetch_port_write.translated <= fetch_address + 4;
            -- Put our Core ID in the upper bits
            fetch_port_write.user_flags(7 downto 6) <=
              std_logic_vector(to_unsigned(coreid+1,2));
            -- Mark transaction as being instruction fetch
            fetch_port_write.user_flags(5) <= '0';
            -- And finally the transaction number.
            fetch_port_write.user_flags(4 downto 0)
              <= std_logic_vector(ifetch_transaction_counter);
            -- Now update our transaction numbers
            ifetch_transaction_counter <= ifetch_transaction_counter + 1;
            fetch_address <= fetch_address + 4;
            fetch_port_used := true;
            if (burst_add_one = false) then
              report "I-FETCH" & integer'image(coreid)
                & " : Decrementing burst_fetch to "
                & integer'image(burst_fetch-1) & ", fetching $"
                & to_hstring(fetch_address + 4) & ", desired_address=$"
                & to_hstring(desired_address);
              burst_fetch <= burst_fetch - 1;
            else
              report "I-FETCH" & integer'image(coreid)
                & " : Holding burst_fetch";
            end if;
          else
            report "I-FETCH" & integer'image(coreid)
              & " : FETCH port NOT ready, so holding burst_fetch";
          end if;          
        elsif (burst_add_one = true) then
          report "I-FETCH" & integer'image(coreid)
            & " : Incrementing burst_fetch to " & integer'image(burst_fetch + 1);
          if burst_fetch < (BYTE_BUFFER_WIDTH/4+1) then
            burst_fetch <= burst_fetch + 1;
          end if;
        end if;
        -- Make sure that we don't get stuck forever waiting for bytes
        -- XXX This should never be needed, and just results in a core placing
        -- unnecessary demands on the memory bandwidth
        -- if (bytes_ready < 4) and (burst_fetch = 0) then
        -- report "I-FETCH" & integer'image(coreid)
        -- & " : Empty buffer: resetting burst_fetch to " & integer'image((BYTE_BUFFER_WIDTH/4+1));
        -- burst_fetch <= (BYTE_BUFFER_WIDTH/4+1);
        -- end if;
        
        report "I-FETCH" & integer'image(coreid)
          & " buffer was " & to_hstring(byte_buffer)
          &", now " & to_hstring(new_byte_buffer)
          &", with " & integer'image(new_bytes_ready)
          & " bytes ready, " & integer'image(consumed_bytes) & " bytes consumed "
          & "(burst_fetch = " & integer'image(burst_fetch) & ").";

        byte_buffer <= new_byte_buffer;
        ilen_buffer <= new_ilen_buffer;
        bytes_ready <= new_bytes_ready;
        if new_bytes_ready < (BYTE_BUFFER_WIDTH-4) then
          space_for_bytes <= true;
        else
          space_for_bytes <= false;
        end if;
        buffer_address <= buffer_address + consumed_bytes;

        report "I-FETCH" & integer'image(coreid)
          & " : Updated: new_bytes_ready = " & integer'image(new_bytes_ready)
          & ", consumed_bytes = " & integer'image(consumed_bytes);

        instruction_address <= instruction_address + consumed_bytes;
        instruction_pc <= instruction_pc + consumed_bytes;        

      end if;
      
      if (address_redirecting = true) and (reset = '0') then
        report "$" & to_hstring(instruction_address) &
          " PREFETCH" & integer'image(coreid)
          & " : "
          & "redirection requested to $" & to_hstring(redirected_address);

        -- Starting to pursue a new trace, fetch until we hit a non-conditional
        -- branch.
        end_of_trace <= false;
        
        instruction_address <= redirected_address;
        instruction_pc(15 downto 8) <= redirected_pch;
        instruction_pc(7 downto 0) <= redirected_address(7 downto 0);

        -- Invalidate current buffer
        bytes_ready <= 0;
        space_for_bytes <= true;
        -- And remember that we can fetch several words at once.
        -- Enough to fill, plus one waiting in the wings.
        burst_fetch <= (BYTE_BUFFER_WIDTH / 4) + 1;
        -- And reset the bytes eaten counter that we use to decide when to load
        -- the next word.
        dispatched_bytes <= 0;

        -- Start reading from this address.
        -- Clobber any other address we have asked for, as anything else we
        -- were waiting for is not redundant.
        fetch_port_write.valid <= true;
        fetch_port_write.translated <= redirected_address;
        fetch_port_used := true;
        fetch_port_write.user_flags <=
          std_logic_vector(to_unsigned(coreid+1,2)&"0"&
                           ifetch_transaction_counter);
        report "FETCH" & integer'image(coreid)
          & " port ready: Due to redirection, asking for $"
          & to_hstring(redirected_address)
          & " as Tid $" & to_hstring(to_unsigned(coreid+1,2)&"0"&
                                     ifetch_transaction_counter);

        -- Set the transaction ID we will be waiting for for this data
        ifetch_expected_transaction_counter <= ifetch_transaction_counter;
        ifetch_transaction_counter          <= ifetch_transaction_counter + 1;
        
        fetch_address <= redirected_address;
        desired_address <= redirected_address;
        report "I-FETCH" & integer'image(coreid)
          & " : desired_address <= $" & to_hstring(redirected_address);

      else
      -- Otherwise, keep fetching from where we were.
      end if;

      -- Work out whether we can request more instructions next cycle?
      -- This approach avoids the need for any buffers, but it does mean that
      -- we can only fetch an instruction every other cycle.  However, for Core0,
      -- we know that we have the highest priority access to the memory controller,
      -- so we can always fetch.
      -- For the other cores, this means that we are limited to fetching at
      -- most 2 instruction bytes per cycle on average (4 per 2 cycles), so
      -- there will be some pipeline stalling due to fetch delays on those
      -- cores if they use too many 3-byte instructions in rapid succession.
      -- This could be avoided by buffering the fetch ports 1 - 3, so that we
      -- get a one-cycle warning, and can stop fetching when we know that it is
      -- busy.  The trade-off would then be one extra cycle of instruction
      -- fetch latency on ports 1-3.  We can work out the best trade-off there
      -- later.
      fetched_last_cycle <= fetch_port_used;
      if (coreid = 0) and primary_core_boost then
        if (not fetch_port_used) or (reset='1') then
          fetch_port_write.valid <= false;
        end if;
      elsif reset='1' then
        fetch_port_write.valid <= false;        
      end if;
      
      report "$" & to_hstring(instruction_address) & " I-FETCH" & integer'image(coreid);

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

      if  (address_redirecting = false) and
        (instruction.instruction_flags.do_branch
         and (not instruction.instruction_flags.do_branch_conditional)) then
        -- Found a non-conditional branch instruction, so there is no point continuing
        -- to burst fetch
        burst_fetch <= 0;
        end_of_trace <= true;
      end if; 
      
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
        to_unsigned(65538 + to_integer(instruction_pc) + to_integer(
          byte_buffer(15)&byte_buffer(15)&byte_buffer(15)&byte_buffer(15)&
          byte_buffer(15)&byte_buffer(15)&byte_buffer(15)&byte_buffer(15)&
          byte_buffer(15 downto 8)),16);
      branch16_pc <=
        to_unsigned(65539 + to_integer(instruction_pc) + to_integer(byte_buffer(23 downto 8)),16);
      -- For those bizarre BBR/BBS instructions, where the branch is from the
      -- 3rd byte of the instruction, not the 2nd
      branch8_zp_pc <=
        to_unsigned(65539 + to_integer(instruction_pc) + to_integer(
          byte_buffer(23)&byte_buffer(23)&byte_buffer(23)&byte_buffer(23)&
          byte_buffer(23)&byte_buffer(23)&byte_buffer(23)&byte_buffer(23)&
          byte_buffer(23 downto 16)),16);
      
      instruction_out <= instruction;
    end if;
  end process;

end behavioural;
