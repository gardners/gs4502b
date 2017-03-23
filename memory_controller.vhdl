-- GS4502B Memory Controller
--
-- The memory controller has 4 x 8-bit RAMs, each independently addressable, so
-- that non-aligned reads and writes can be easily handled.
--
-- The memories are also dual-port. One port is used primarily for instruction
-- fetching (fetch ops), while the other is used for memory accesses performed by
-- instructions (mem ops).  Mem ops can also include RMW ALU operations, and
-- operate on a transactional model that allows the main part of the CPU to be
-- notified when a memory access completes, and be provided with the resulting
-- value and revised CPU flags. Mem ops can also reference other in-flight mem
-- ops, so that an LDA $nnnn / STA $nnnn sequence can execute without blocking
-- the main part of the cpu.  In this way, this memory controller acts as half
-- of the register-renaming unit of the CPU.
--

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

entity memory_controller is
  port (
    cpuclock : in std_logic;
    ioclock : in std_logic;
    primary_core_boost : in boolean;

    entity_name : in string;

    -- Fastio interface
    fastio_address : out unsigned(19 downto 0) := (others => '0');
    fastio_rdata : in unsigned(7 downto 0);
    fastio_wdata : out unsigned(7 downto 0) := x"FF";
    fastio_read : out std_logic := '0';
    fastio_write : out std_logic := '0';
    
    -- We offer four memory fetch ports
    -- (3 CPU cores + VIC-IV fetch)
    -- Lower number ports have priority over
    -- higher number ports.  Intended order is:
    -- Core0, VIC-IV, Core1, Core2.
    fetch_port0_in : in fetch_port_in;
    fetch_port0_out : out fetch_port_out;
    fetch_port1_in : in fetch_port_in;
    fetch_port1_out : out fetch_port_out;
    fetch_port2_in : in fetch_port_in;
    fetch_port2_out : out fetch_port_out;
    fetch_port3_in : in fetch_port_in;
    fetch_port3_out : out fetch_port_out;

    -- And three mem op ports for Core0-2.
    -- Again, access is prioritised between them.
    mem_port0_in : mem_port_in;
    mem_port0_out : mem_port_out;
    mem_port1_in : mem_port_in;
    mem_port1_out : mem_port_out;
    mem_port2_in : mem_port_in;
    mem_port2_out : mem_port_out
    );
end memory_controller;

architecture behavioural of memory_controller is

  -- 4x 64KB x 8bit RAMs to make main memory
  -- (Actually, they are 9-bit RAMs.  We aren't currently doing anything with
  -- the 9th bit, but might, for example, us it to mark branch prediction
  -- information.)
  constant IDLE_RAM_INTERFACE : ram_interface := (
    iaddr => (others => '0'),
    maddr => (others => '0'),
    mwrite => '0',
    mwdata => (others => '0')
    );
  type all_ram_interfaces is array (0 to 3) of ram_interface;
  signal ram_interfaces : all_ram_interfaces;
  signal irdata0 : std_logic_vector(8 downto 0);
  signal irdata1 : std_logic_vector(8 downto 0);
  signal irdata2 : std_logic_vector(8 downto 0);
  signal irdata3 : std_logic_vector(8 downto 0);
  signal mrdata0 : std_logic_vector(8 downto 0);
  signal mrdata1 : std_logic_vector(8 downto 0);
  signal mrdata2 : std_logic_vector(8 downto 0);
  signal mrdata3 : std_logic_vector(8 downto 0);

  signal next_fetch_address0 : translated_address;
  signal next_fetch_address1 : translated_address;
  signal next_fetch_address2 : translated_address;
  signal next_fetch_address3 : translated_address;
  signal next_fetch_flags : std_logic_vector(7 downto 0);
  signal next_fetch_port : integer range 0 to 3;
  signal bram_fetch_address_in : translated_address;
  signal bram_fetch_flags_in : std_logic_vector(7 downto 0);
  signal bram_fetch_port_in : integer range 0 to 3;
  signal bram_fetch_address_1 : translated_address;
  signal bram_fetch_flags_1 : std_logic_vector(7 downto 0);
  signal bram_fetch_port_1 : integer range 0 to 3;
  signal bram_fetch_address_out : translated_address;
  signal bram_fetch_flags_out : std_logic_vector(7 downto 0);
  signal bram_fetch_port_out : integer range 0 to 3;
  signal bram_bytes_out : bytes4;

  
  signal port2_ist_dran : boolean := false;
  signal port0_wasnt_last : boolean := true;

  -- Buffer all request ports so that we don't lose any requests
  signal fetch_port0_buffer : fetch_port_in;
  signal fetch_port1_buffer : fetch_port_in;
  signal fetch_port2_buffer : fetch_port_in;
  signal fetch_port3_buffer : fetch_port_in;
  signal fetch_port0_buffering : boolean := false;
  signal fetch_port1_buffering : boolean := false;
  signal fetch_port2_buffering : boolean := false;
  signal fetch_port3_buffering : boolean := false;  


  function bytes_reorder(v : bytes4; rotate : unsigned(1 downto 0)) return bytes4 is
      variable vout : bytes4;
  begin
    case rotate is
      when "00" =>
        vout := v;
      when "01" =>
        vout(0) := v(1);
        vout(1) := v(2);
        vout(2) := v(3);
        vout(3) := v(0);
      when "10" =>
        vout(0) := v(2);
        vout(1) := v(3);
        vout(2) := v(0);
        vout(3) := v(1);
      when others =>
        vout(0) := v(2);
        vout(1) := v(3);
        vout(2) := v(0);
        vout(3) := v(1);
    end case;

    report "bytes_reorder: reordering (" &
      "$" & to_hstring(v(0)) & "," &
      "$" & to_hstring(v(1)) & "," &
      "$" & to_hstring(v(2)) & "," &
      "$" & to_hstring(v(3)) & ") to (" &
      "$" & to_hstring(vout(0)) & "," &
      "$" & to_hstring(vout(1)) & "," &
      "$" & to_hstring(vout(2)) & "," &
      "$" & to_hstring(vout(3)) & ") (rotate=" & integer'image(to_integer(rotate))
      & ")";

    return vout;
  end function;

  constant ram0name : string :=  entity_name & ".ram0";
  constant ram1name : string :=  entity_name & ".ram1";
  constant ram2name : string :=  entity_name & ".ram2";
  constant ram3name : string :=  entity_name & ".ram3";
  
begin      

  
  ram0: entity work.ram0
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_interfaces(0).iaddr,
               a_din => (others => '0'),
               a_dout => irdata0,

               entity_name => ram0name,
               
               b_clk => cpuclock,
               b_wr => ram_interfaces(0).mwrite,
               b_addr => ram_interfaces(0).maddr,
               b_din => ram_interfaces(0).mwdata,
               b_dout => mrdata0
               );
  
  ram1: entity work.ram1
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_interfaces(1).iaddr,
               a_din => (others => '0'),
               a_dout => irdata1,

               entity_name => ram1name,

               b_clk => cpuclock,
               b_wr => ram_interfaces(1).mwrite,
               b_addr => ram_interfaces(1).maddr,
               b_din => ram_interfaces(1).mwdata,
               b_dout => mrdata1
               );
  
  ram2: entity work.ram2
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_interfaces(2).iaddr,
               a_din => (others => '0'),
               a_dout => irdata2,

               entity_name => ram2name,

               b_clk => cpuclock,
               b_wr => ram_interfaces(2).mwrite,
               b_addr => ram_interfaces(2).maddr,
               b_din => ram_interfaces(2).mwdata,
               b_dout => mrdata2
               );
  
  ram3: entity work.ram3
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_interfaces(3).iaddr,
               a_din => (others => '0'),
               a_dout => irdata3,

               entity_name => ram3name,

               b_clk => cpuclock,
               b_wr => ram_interfaces(3).mwrite,
               b_addr => ram_interfaces(3).maddr,
               b_din => ram_interfaces(3).mwdata,
               b_dout => mrdata3
               );
  
  process (cpuclock, ioclock) is
    variable fetch_address : translated_address;
    variable fetch_port_number : integer range 0 to 3;
    variable fetch_flags : std_logic_vector(7 downto 0);
    variable fetching : boolean := true;
    variable fetch_port0 : fetch_port_in;
    variable fetch_port1 : fetch_port_in;
    variable fetch_port2 : fetch_port_in;
    variable fetch_port3 : fetch_port_in;
    
  begin
    
    if rising_edge(cpuclock) then

      -- XXX Make second ports idle until we implement memory access
      for i in 0 to 3 loop
        ram_interfaces(i).maddr <= (others => '0');
        ram_interfaces(i).mwrite <= '0';
      end loop;
      
      -- Check for activity on the fetch ports
      report "Fetch port valids = ("
        & boolean'image(fetch_port0_in.valid) & ","
        & boolean'image(fetch_port1_in.valid) & ","
        & boolean'image(fetch_port2_in.valid) & ","
        & boolean'image(fetch_port3_in.valid) & ").";
      report "MEM_CONTROL: Port0 valid="
        & boolean'image(fetch_port0_in.valid)
        & ", address $" & to_hstring(fetch_port0_in.translated); 
      report "MEM_CONTROL: Port0 buffer valid="
        & boolean'image(fetch_port0_buffer.valid)
        & ", address $" & to_hstring(fetch_port0_buffer.translated)
        & " (buffering=" & boolean'imagE(fetch_port0_buffering);
      report "MEM_CONTROL: Port1 valid="
        & boolean'image(fetch_port1_in.valid)
        & ", address $" & to_hstring(fetch_port1_in.translated); 
      report "MEM_CONTROL: Port2 valid="
        & boolean'image(fetch_port2_in.valid)
        & ", address $" & to_hstring(fetch_port2_in.translated); 
      report "MEM_CONTROL: Port3 valid="
        & boolean'image(fetch_port3_in.valid)
        & ", address $" & to_hstring(fetch_port3_in.translated); 
      report "MEM_CONTROL: Port3 buffer valid="
        & boolean'image(fetch_port3_buffer.valid)
        & ", address $" & to_hstring(fetch_port3_buffer.translated)
        & " (buffering=" & boolean'imagE(fetch_port3_buffering);

      if fetch_port0_buffering then
        fetch_port0 := fetch_port0_buffer;
      else
        fetch_port0 := fetch_port0_in;
      end if;
      if fetch_port1_buffering then
        fetch_port1 := fetch_port1_buffer;
      else
        fetch_port1 := fetch_port1_in;
      end if;
      if fetch_port2_buffering then
        fetch_port2 := fetch_port2_buffer;
      else
        fetch_port2 := fetch_port2_in;
      end if;
      if fetch_port3_buffering then
        fetch_port3 := fetch_port3_buffer;
      else
        fetch_port3 := fetch_port3_in;
      end if;
      if fetch_port0_in.valid and (not fetch_port0_buffering) then
        fetch_port0_buffer <= fetch_port0_in;
        fetch_port0_buffering <= true;
        fetch_port0_out.ready <= false;
      end if;
      if fetch_port1_in.valid and (not fetch_port1_buffering) then
        fetch_port1_buffer <= fetch_port1_in;
        fetch_port1_buffering <= true;
        fetch_port1_out.ready <= false;
      end if;
      if fetch_port2_in.valid and (not fetch_port2_buffering) then
        fetch_port2_buffer <= fetch_port2_in;
        fetch_port2_buffering <= true;
        fetch_port2_out.ready <= false;
      end if;
      if fetch_port3_in.valid and (not fetch_port3_buffering) then
        fetch_port3_buffer <= fetch_port3_in;
        fetch_port3_buffering <= true;
        fetch_port3_out.ready <= false;
      end if;
      
      if fetch_port0.valid and (port0_wasnt_last or primary_core_boost) then
        port0_wasnt_last <= false;
        fetching := true;
        fetch_address := fetch_port0.translated;
        fetch_flags := fetch_port0.user_flags;
        fetch_port_number := 0;
        fetch_port0_buffering <= false;
        fetch_port0_out.ready <= fetch_port0_buffering;
      elsif fetch_port1.valid then
        port0_wasnt_last <= true;
        fetching := true;
        fetch_address := fetch_port1.translated;
        fetch_flags := fetch_port1.user_flags;
        fetch_port_number := 1;
        fetch_port1_buffering <= false;
        fetch_port1_out.ready <= fetch_port1_buffering;
      elsif fetch_port2.valid and ((not fetch_port3.valid) or port2_ist_dran) then
        port0_wasnt_last <= true;
        fetching := true;
        fetch_address := fetch_port2.translated;
        fetch_flags := fetch_port2.user_flags;
        fetch_port_number := 2;
        port2_ist_dran <= false;
        fetch_port2_buffering <= false;
        fetch_port2_out.ready <= fetch_port2_buffering;
      elsif fetch_port3.valid then
        port0_wasnt_last <= true;
        fetching := true;
        fetch_address := fetch_port3.translated;
        fetch_flags := fetch_port3.user_flags;
        fetch_port_number := 3;
        port2_ist_dran <= true;
        fetch_port3_buffering <= false;
        fetch_port3_out.ready <= fetch_port3_buffering;
      else
        port0_wasnt_last <= true;
        fetching := false;
      end if;      
      
      if fetching then
        report "MEM_CONTROLLER : Fetch port " & integer'image(fetch_port_number)
          & " is asking for address $" & to_hstring(fetch_address)
          & " (Tid = $" & to_hstring(fetch_flags) & ").";

        -- Feed request into memory
        -- XXX - Add support for unaligned requests
        -- XXX - Add support for non-BRAM requests (i.e., IO, and later, DDR RAM)
        case fetch_address(1 downto 0) is
          when "00" =>
            next_fetch_address0 <= to_unsigned(to_integer(fetch_address) + 0,32);
            next_fetch_address1 <= to_unsigned(to_integer(fetch_address) + 0,32);
            next_fetch_address2 <= to_unsigned(to_integer(fetch_address) + 0,32);
          when "01" =>
            next_fetch_address0 <= to_unsigned(to_integer(fetch_address) + 4,32);
            next_fetch_address1 <= to_unsigned(to_integer(fetch_address) + 0,32);
            next_fetch_address2 <= to_unsigned(to_integer(fetch_address) + 0,32);
          when "10" =>
            next_fetch_address0 <= to_unsigned(to_integer(fetch_address) + 4,32);
            next_fetch_address1 <= to_unsigned(to_integer(fetch_address) + 4,32);
            next_fetch_address2 <= to_unsigned(to_integer(fetch_address) + 0,32);
          when others =>
            next_fetch_address0 <= to_unsigned(to_integer(fetch_address) + 4,32);
            next_fetch_address1 <= to_unsigned(to_integer(fetch_address) + 4,32);
            next_fetch_address2 <= to_unsigned(to_integer(fetch_address) + 4,32);
        end case;          
        next_fetch_address3 <= to_unsigned(to_integer(fetch_address) + 0,32);
        next_fetch_port <= fetch_port_number;
        next_fetch_flags <= fetch_flags;        
      else
        report "MEM_CONTROLLER : Not fetching.";
        next_fetch_address0 <= (others => '0');
        next_fetch_address1 <= (others => '0');
        next_fetch_address2 <= (others => '0');
        next_fetch_address3 <= (others => '0');
      end if;

      -- Push request to RAM
      ram_interfaces(0).iaddr <= std_logic_vector(next_fetch_address0(18 downto 2));
      ram_interfaces(1).iaddr <= std_logic_vector(next_fetch_address1(18 downto 2));
      ram_interfaces(2).iaddr <= std_logic_vector(next_fetch_address2(18 downto 2));
      ram_interfaces(3).iaddr <= std_logic_vector(next_fetch_address3(18 downto 2));
      
      bram_fetch_address_in <= next_fetch_address0;
      bram_fetch_flags_in <= next_fetch_flags;
      bram_fetch_port_in <= next_fetch_port;

      -- Progress BRAM requests through (BRAM takes two cycles to service a read)
      bram_fetch_address_1 <= bram_fetch_address_in;
      bram_fetch_flags_1 <= bram_fetch_flags_in;
      bram_fetch_port_1 <= bram_fetch_port_in;

      bram_fetch_address_out <= bram_fetch_address_1;
      bram_fetch_flags_out <= bram_fetch_flags_1;
      bram_fetch_port_out <= bram_fetch_port_1;

      bram_bytes_out(0) <= irdata0;
      bram_bytes_out(1) <= irdata1;
      bram_bytes_out(2) <= irdata2;
      bram_bytes_out(3) <= irdata3;

      report "MEM_CONTROLLER : Presenting address $"
        & to_hstring(bram_fetch_address_out)
        & " ($" & to_hstring(bram_bytes_out(0))
        & ",$" & to_hstring(bram_bytes_out(1))
        & ",$" & to_hstring(bram_bytes_out(2))
        & ",$" & to_hstring(bram_bytes_out(3))
        & ") to port " & integer'image(bram_fetch_port_out)
        & " (Tid $" & to_hstring(bram_fetch_flags_out) & ").";

      -- By default, present BRAM read result to everyone, in case they want it,
      -- and also to minimise logic depth.  Where non-BRAM reads occur, they will
      -- be used to overwrite these value.
      fetch_port0_out.translated <= bram_fetch_address_out;
      fetch_port0_out.bytes
        <= bytes_reorder(bram_bytes_out,bram_fetch_address_out(1 downto 0));
      fetch_port0_out.user_flags <= bram_fetch_flags_out;
      fetch_port1_out.translated <= bram_fetch_address_out;
      fetch_port1_out.bytes
        <= bytes_reorder(bram_bytes_out,bram_fetch_address_out(1 downto 0));
      fetch_port1_out.user_flags <= bram_fetch_flags_out;
      fetch_port2_out.translated <= bram_fetch_address_out;
      fetch_port2_out.bytes
        <= bytes_reorder(bram_bytes_out,bram_fetch_address_out(1 downto 0));
      fetch_port2_out.user_flags <= bram_fetch_flags_out;
      fetch_port3_out.translated <= bram_fetch_address_out;
      fetch_port3_out.bytes
        <= bytes_reorder(bram_bytes_out,bram_fetch_address_out(1 downto 0));
      fetch_port3_out.user_flags <= bram_fetch_flags_out;
      
    end if;
    end process;
  
end behavioural;

