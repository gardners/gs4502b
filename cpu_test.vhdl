library ieee;
USE ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;
use work.all;
use work.debugtools.all;
use work.types.all;
use work.instruction_types.all;
use work.instruction_equations.all;
use work.extra_instruction_equations.all;


entity cpu_test is
  
end cpu_test;

architecture behavior of cpu_test is

  signal pixelclock : std_logic := '0';
  signal cpuclock : std_logic := '0';
  signal ioclock : std_logic := '0';
  signal reset : std_logic := '0';
  signal irq : std_logic := '1';
  signal nmi : std_logic := '1';

  signal monitor_pc : unsigned(15 downto 0);

  signal fetch_port_read : fetch_port_out;
  signal fetch_port_write : fetch_port_in;

  signal fastio_rdata : unsigned(7 downto 0) := x"00";
  signal fastio_wdata : unsigned(7 downto 0) := x"00";
  signal fastio_address : unsigned(19 downto 0);
  signal fastio_write : std_logic;
  signal fastio_read : std_logic;

  signal monitor_core_id : unsigned(1 downto 0) := "00";
  
begin

  -- instantiate components here
  core0: entity work.gs4502b
    port map (
      cpuclock => cpuclock,
      ioclock => ioclock,
      reset => reset,

      -- Fetch memory interface for VIC-IV
      fetch_port_read => fetch_port_read,
      fetch_port_write => fetch_port_write,

      -- Fast IO interface for CPU
      fastio_address => fastio_address,
      fastio_rdata => fastio_rdata,
      fastio_wdata => fastio_wdata,
      fastio_read => fastio_read,
      fastio_write => fastio_write,

      monitor_core_id => monitor_core_id,
      monitor_pc => monitor_pc,
      
      rom_at_8000 => '0',
      rom_at_a000 => '0',
      rom_at_c000 => '0',
      rom_at_e000 => '0',
      viciii_iomode => "11"
      );
  
  process
  begin  -- process tb
    report "beginning simulation" severity note;

    -- Assert reset for 16 cycles to give CPU pipeline ample time to flush out
    -- and actually reset.
    reset <= '1';
    for i in 1 to 4 loop
      pixelclock <= '1'; cpuclock <= '1'; ioclock <= '1'; wait for 2.5 ns;     
      pixelclock <= '0'; cpuclock <= '0'; ioclock <= '1'; wait for 2.5 ns;     
      pixelclock <= '1'; cpuclock <= '1'; ioclock <= '1'; wait for 2.5 ns;     
      pixelclock <= '0'; cpuclock <= '0'; ioclock <= '1'; wait for 2.5 ns;     
      pixelclock <= '1'; cpuclock <= '1'; ioclock <= '0'; wait for 2.5 ns;     
      pixelclock <= '0'; cpuclock <= '0'; ioclock <= '0'; wait for 2.5 ns;     
      pixelclock <= '1'; cpuclock <= '1'; ioclock <= '0'; wait for 2.5 ns;     
      pixelclock <= '0'; cpuclock <= '0'; ioclock <= '0'; wait for 2.5 ns;     
    end loop;  -- i
    -- Run CPU for 8 million cycles.
    reset <= '0';
    for i in 1 to 2000000 loop
      pixelclock <= '1'; cpuclock <= '1'; ioclock <= '1'; wait for 2.5 ns;     
      pixelclock <= '0'; cpuclock <= '0'; ioclock <= '1'; wait for 2.5 ns;     
      pixelclock <= '1'; cpuclock <= '1'; ioclock <= '1'; wait for 2.5 ns;     
      pixelclock <= '0'; cpuclock <= '0'; ioclock <= '1'; wait for 2.5 ns;     
      pixelclock <= '1'; cpuclock <= '1'; ioclock <= '0'; wait for 2.5 ns;     
      pixelclock <= '0'; cpuclock <= '0'; ioclock <= '0'; wait for 2.5 ns;     
      pixelclock <= '1'; cpuclock <= '1'; ioclock <= '0'; wait for 2.5 ns;     
      pixelclock <= '0'; cpuclock <= '0'; ioclock <= '0'; wait for 2.5 ns;     
    end loop;  -- i
    assert false report "End of simulation" severity failure;
  end process;

  
end behavior;

