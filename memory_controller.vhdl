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
    irdata => (others => '0'),
    maddr => (others => '0'),
    mwrite => '0',
    mwdata => (others => '0'),
    mrdata => (others => '0')
    );
  type all_ram_interfaces is array (0 to 3) of ram_interface;
  signal ram_interfaces : all_ram_interfaces := ( others => IDLE_RAM_INTERFACE);

  signal port2_ist_dran : boolean := false;
  
begin    
  
  ram: entity work.ram0
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_interfaces(0).iaddr,
               a_din => (others => '0'),
               a_dout => ram_interfaces(0).irdata,

               b_clk => cpuclock,
               b_wr => ram_interfaces(0).mwrite,
               b_addr => ram_interfaces(0).maddr,
               b_din => ram_interfaces(0).mwdata,
               b_dout => ram_interfaces(0).mrdata
               );
  
  ram1: entity work.ram1
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_interfaces(1).iaddr,
               a_din => (others => '0'),
               a_dout => ram_interfaces(1).irdata,

               b_clk => cpuclock,
               b_wr => ram_interfaces(1).mwrite,
               b_addr => ram_interfaces(1).maddr,
               b_din => ram_interfaces(1).mwdata,
               b_dout => ram_interfaces(1).mrdata
               );
  
  ram2: entity work.ram2
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_interfaces(2).iaddr,
               a_din => (others => '0'),
               a_dout => ram_interfaces(2).irdata,

               b_clk => cpuclock,
               b_wr => ram_interfaces(2).mwrite,
               b_addr => ram_interfaces(2).maddr,
               b_din => ram_interfaces(2).mwdata,
               b_dout => ram_interfaces(2).mrdata
               );
  
  ram3: entity work.ram3
    port map ( a_clk => cpuclock,
               a_wr => '0',
               a_addr => ram_interfaces(3).iaddr,
               a_din => (others => '0'),
               a_dout => ram_interfaces(3).irdata,

               b_clk => cpuclock,
               b_wr => ram_interfaces(3).mwrite,
               b_addr => ram_interfaces(3).maddr,
               b_din => ram_interfaces(3).mwdata,
               b_dout => ram_interfaces(3).mrdata
               );
  
  process (cpuclock, ioclock) is
    variable fetch_address : translated_address;
    variable fetch_port_number : integer range 0 to 3;
    variable fetch_flags : std_logic_vector(7 downto 0);
    variable fetching : boolean := true;
  begin
    if rising_edge(cpuclock) then
      -- Toggle between ports 2 and 3 having priority, so that they share
      -- available bandwidth more fairly
      port2_ist_dran <= not port2_ist_dran;
      
      -- Check for activity on the fetch ports
      report "Fetch port valids = ("
        & boolean'image(fetch_port0_in.valid) & ","
        & boolean'image(fetch_port1_in.valid) & ","
        & boolean'image(fetch_port2_in.valid) & ","
        & boolean'image(fetch_port3_in.valid) & ").";
      
      if fetch_port0_in.valid then
        fetching := true;
        fetch_address := fetch_port0_in.translated;
        fetch_flags := fetch_port0_in.user_flags;
        fetch_port_number := 0;
        fetch_port0_out.acknowledged <= true;
        fetch_port1_out.acknowledged <= false;
        fetch_port2_out.acknowledged <= false;
        fetch_port3_out.acknowledged <= false;
      elsif fetch_port1_in.valid then
        fetching := true;
        fetch_address := fetch_port1_in.translated;
        fetch_flags := fetch_port1_in.user_flags;
        fetch_port_number := 1;
        fetch_port1_out.acknowledged <= true;
        fetch_port0_out.acknowledged <= false;
        fetch_port2_out.acknowledged <= false;
        fetch_port3_out.acknowledged <= false;
      elsif fetch_port2_in.valid and ((not fetch_port3_in.valid) or port2_ist_dran) then
        fetching := true;
        fetch_address := fetch_port2_in.translated;
        fetch_flags := fetch_port2_in.user_flags;
        fetch_port_number := 2;
        fetch_port2_out.acknowledged <= true;
        fetch_port0_out.acknowledged <= false;
        fetch_port1_out.acknowledged <= false;
        fetch_port3_out.acknowledged <= false;
      elsif fetch_port3_in.valid then
        fetching := true;
        fetch_address := fetch_port3_in.translated;
        fetch_flags := fetch_port3_in.user_flags;
        fetch_port_number := 3;
        fetch_port3_out.acknowledged <= true;
        fetch_port0_out.acknowledged <= false;
        fetch_port1_out.acknowledged <= false;
        fetch_port2_out.acknowledged <= false;
      else
        fetching := false;
        fetch_port0_out.acknowledged <= false;
        fetch_port1_out.acknowledged <= false;
        fetch_port2_out.acknowledged <= false;
        fetch_port3_out.acknowledged <= false;
      end if;
      if fetching then
        report "MEM_CONTROLLER : Fetch port " & integer'image(fetch_port_number)
          & " is asking for address $" & to_hstring(fetch_address);
      else
        report "MEM_CONTROLLER : Not fetching.";
      end if;
    end if;
  end process;
  
end behavioural;

