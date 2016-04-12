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
    fetch_port_read : fetch_port_out;
    fetch_port_write : fetch_port_in;

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

  type fetch_ports_in is array(0 to 2) of fetch_port_in;
  type fetch_ports_out is array(0 to 2) of fetch_port_out;
  type mem_ports_in is array(0 to 2) of mem_port_in;
  type mem_ports_out is array(0 to 2) of mem_port_out;

  type irqs is array(0 to 2) of std_logic;
  type nmis is array(0 to 2) of std_logic;
begin
  mem_control: entity work.memory_controller
    port map ( cpuclock => cpuclock,
               ioclock => ioclock,

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
               fetch_port0_in => fetch_port_write,
               fetch_port0_out => fetch_port_read,
               fetch_port0_in => fetch_ports_in(1),
               fetch_port0_out => fetch_ports_out(1),
               fetch_port0_in => fetch_ports_in(2),
               fetch_port0_out => fetch_ports_out(2),

               -- Now memory ports for the three cores
               mem_port_0_in => mem_ports_in(0),
               mem_port_0_out => mem_ports_out(0),
               mem_port_1_in => mem_ports_in(1),
               mem_port_1_out => mem_ports_out(1),
               mem_port_2_in => mem_ports_in(2),
               mem_port_2_out => mem_ports_out(2)
               );

  cpu_cores: for core in 0 to 2 generate
    thecore: entity work.gs4502b_core
      port map ( coreid => core,
                 cpuclock => cpuclock,
                 reset => reset,
                 monitor_pc => monitor_pcs(core),

                 fetch_port_read => fetch_ports_out(core),
                 fetch_port_write => fetch_ports_in(core),
                 mem_port_read => mem_ports_out(core),
                 mem_port_write => mem_ports_in(core),

                 rom_at_8000 => rom_at_8000,
                 rom_at_a000 => rom_at_a000,
                 rom_at_c000 => rom_at_c000,
                 rom_at_e000 => rom_at_e000,
                 viciii_iomode => viciii_iomode
                 );
  end generate cpu_cores;

end behavioural;
  
