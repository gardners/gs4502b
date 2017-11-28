use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.instruction_equations.all;
use work.instruction_lengths.all;
use work.types.all;
use work.visualise.all;

entity gs4502b is
  port (
    cpuclock : in std_logic;
    ioclock : in std_logic;

    reset : in std_logic;
    
    -- Interrupts need to be identified by source, since we will allow
    -- directing each interrupt source separately to each core.
    -- XXX add interrupt sources here
    
    -- Fetch port for VIC-IV (or anything else) to access
    -- main memory.
    fetch_port_read : out fetch_port_out;
    fetch_port_write : in fetch_port_in;

    -- Signals we need from VIC-IV to modify memory mapping
    rom_at_8000 : in std_logic;
    rom_at_a000 : in std_logic;
    rom_at_c000 : in std_logic;
    rom_at_e000 : in std_logic;
    viciii_iomode : in std_logic_vector(1 downto 0);
    
    -- Fastio interface
    fastio_address : out unsigned(19 downto 0);
    fastio_rdata : in unsigned(7 downto 0);
    fastio_wdata : out unsigned(7 downto 0);
    fastio_read : out std_logic;
    fastio_write : out std_logic;

    -- Monitor interface (now includes core select line)
    monitor_core_id : in unsigned(1 downto 0);
    monitor_pc : out unsigned(15 downto 0)

    );
end gs4502b;

architecture behavioural of gs4502b is

  type fetch_ports_in_t is array(0 to 2) of fetch_port_in;
  type fetch_ports_out_t is array(0 to 2) of fetch_port_out;
  type mem_ports_in_t is array(0 to 2) of mem_port_in;
  type mem_ports_out_t is array(0 to 2) of mem_port_out;
  signal fetch_ports_in : fetch_ports_in_t;
  signal fetch_ports_out : fetch_ports_out_t;
  signal mem_ports_in : mem_ports_in_t;
  signal mem_ports_out : mem_ports_out_t;

  -- When true, this allows the primary CPU core (core 0) to monopolise the
  -- instruction fetch memory bus, potentially starving the other cores of
  -- instruction fetch cycles.  
  signal primary_core_boost : boolean := false;

  type pc3 is array (0 to 2) of unsigned(15 downto 0);
  signal monitor_pcs : pc3;
  
  type irqs is array(0 to 2) of std_logic;
  type nmis is array(0 to 2) of std_logic;
begin

  -- Tell outside world what is happening
  monitor_pc <= monitor_pcs(to_integer(monitor_core_id));
  
  mem_control: entity work.memory_controller
    generic map ( entity_name => "gs4502b.mem_control" )

    port map ( cpuclock => cpuclock,
               ioclock => ioclock,
               primary_core_boost => primary_core_boost,

               -- FastIO interface
               fastio_address => fastio_address,
               fastio_rdata => fastio_rdata,
               fastio_wdata => fastio_wdata,
               fastio_read => fastio_read,
               fastio_write => fastio_write,
               
               -- Fetch ports, which are priotisied.
               -- Port 0 : Core 0, highest priority
               -- Port 1 : VIC-IV
               -- Port 2 : Core 1
               -- Port 3 : Core 2
               fetch_port0_in => fetch_ports_in(0),
               fetch_port0_out => fetch_ports_out(0),
               fetch_port1_in => fetch_port_write,
               fetch_port1_out => fetch_port_read,
               fetch_port2_in => fetch_ports_in(1),
               fetch_port2_out => fetch_ports_out(1),
               fetch_port3_in => fetch_ports_in(2),
               fetch_port3_out => fetch_ports_out(2),

               -- Now memory ports for the three cores
               mem_port0_in => mem_ports_in(0),
               mem_port0_out => mem_ports_out(0),
               mem_port1_in => mem_ports_in(1),
               mem_port1_out => mem_ports_out(1),
               mem_port2_in => mem_ports_in(2),
               mem_port2_out => mem_ports_out(2)
               );

    core0: entity work.gs4502b_core
      port map ( coreid => 0,
                 entity_name => "gs4502b.core0",
                 cpuclock => cpuclock,                 
                 reset => reset,
                 primary_core_boost => primary_core_boost,
                 
                 monitor_pc => monitor_pcs(0),

                 fetch_port_read => fetch_ports_out(0),
                 fetch_port_write => fetch_ports_in(0),
                 mem_port_read => mem_ports_out(0),
                 mem_port_write => mem_ports_in(0),

                 rom_at_8000 => rom_at_8000,
                 rom_at_a000 => rom_at_a000,
                 rom_at_c000 => rom_at_c000,
                 rom_at_e000 => rom_at_e000,
                 viciii_iomode => viciii_iomode
                 );
    core1: entity work.gs4502b_core
      port map ( coreid => 1,
                 entity_name => "gs4502b.core1",
                 cpuclock => cpuclock,                 
                 reset => reset,
                 primary_core_boost => primary_core_boost,
                 
                 monitor_pc => monitor_pcs(1),

                 fetch_port_read => fetch_ports_out(1),
                 fetch_port_write => fetch_ports_in(1),
                 mem_port_read => mem_ports_out(1),
                 mem_port_write => mem_ports_in(1),

                 rom_at_8000 => rom_at_8000,
                 rom_at_a000 => rom_at_a000,
                 rom_at_c000 => rom_at_c000,
                 rom_at_e000 => rom_at_e000,
                 viciii_iomode => viciii_iomode
                 );
    core2: entity work.gs4502b_core
      port map ( coreid => 2,
                 entity_name => "gs4502b.core2",
                 cpuclock => cpuclock,                 
                 reset => reset,
                 primary_core_boost => primary_core_boost,
                 
                 monitor_pc => monitor_pcs(2),

                 fetch_port_read => fetch_ports_out(2),
                 fetch_port_write => fetch_ports_in(2),
                 mem_port_read => mem_ports_out(2),
                 mem_port_write => mem_ports_in(2),

                 rom_at_8000 => rom_at_8000,
                 rom_at_a000 => rom_at_a000,
                 rom_at_c000 => rom_at_c000,
                 rom_at_e000 => rom_at_e000,
                 viciii_iomode => viciii_iomode
                 );

  process (cpuclock) is
    variable ignored : boolean;
  begin
    if rising_edge(cpuclock) then
      ignored := visualise("gs4502b","cpuclock",cpuclock);
      ignored := visualise("gs4502b","ioclock",ioclock);
      ignored := visualise("gs4502b","reset",reset);
      ignored := visualise("gs4502b","fetch_port_write",fetch_port_write);
      ignored := visualise("gs4502b","rom_at_8000",rom_at_8000);
      ignored := visualise("gs4502b","rom_at_a000",rom_at_a000);
      ignored := visualise("gs4502b","rom_at_c000",rom_at_c000);
      ignored := visualise("gs4502b","rom_at_e000",rom_at_e000);
      ignored := visualise("gs4502b","viciii_iomode",viciii_iomode);
      ignored := visualise("gs4502b","fastio_rdata",fastio_rdata);
      ignored := visualise("gs4502b","monitor_core_id",monitor_core_id);
      ignored := visualise("gs4502b","fetch_ports_in(0)",fetch_ports_in(0));
      ignored := visualise("gs4502b","fetch_ports_in(1)",fetch_ports_in(1));
      ignored := visualise("gs4502b","fetch_ports_in(2)",fetch_ports_in(2));
      ignored := visualise("gs4502b","fetch_ports_out(0)",fetch_ports_out(0));
      ignored := visualise("gs4502b","fetch_ports_out(1)",fetch_ports_out(1));
      ignored := visualise("gs4502b","fetch_ports_out(2)",fetch_ports_out(2));
      ignored := visualise("gs4502b","mem_ports_in(0)",mem_ports_in(0));
      ignored := visualise("gs4502b","mem_ports_in(1)",mem_ports_in(1));
      ignored := visualise("gs4502b","mem_ports_in(2)",mem_ports_in(2));
      ignored := visualise("gs4502b","mem_ports_out(0)",mem_ports_out(0));
      ignored := visualise("gs4502b","mem_ports_out(1)",mem_ports_out(1));
      ignored := visualise("gs4502b","mem_ports_out(2)",mem_ports_out(2));
      ignored := visualise("gs4502b","primary_core_boost",
                           to_std_logic(primary_core_boost));
      ignored := visualise("gs4502b","monitor_pcs(0)",monitor_pcs(0));
      ignored := visualise("gs4502b","monitor_pcs(1)",monitor_pcs(1));
      ignored := visualise("gs4502b","monitor_pcs(2)",monitor_pcs(2));
    end if;
  end process;  
end behavioural;
  
