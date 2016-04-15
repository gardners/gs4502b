----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    22:30:37 12/10/2013 
-- Design Name: 
-- Module Name:    container - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.alu.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity container is
  Port ( CLK_IN : STD_LOGIC;         
         btnCpuReset : in  STD_LOGIC;
--         irq : in  STD_LOGIC;
--         nmi : in  STD_LOGIC;

         ----------------------------------------------------------------------
         -- VGA output
         ----------------------------------------------------------------------
         vsync : out  STD_LOGIC;
         hsync : out  STD_LOGIC;
         vgared : out  UNSIGNED (3 downto 0);
         vgagreen : out  UNSIGNED (3 downto 0);
         vgablue : out  UNSIGNED (3 downto 0);

         ----------------------------------------------------------------------
         -- Debug interfaces on Nexys4 board
         ----------------------------------------------------------------------
         led : out std_logic_vector(15 downto 0);
         sw : in std_logic_vector(15 downto 0);
         btn : in std_logic_vector(4 downto 0);
         
         sseg_ca : out std_logic_vector(7 downto 0);
         sseg_an : out std_logic_vector(7 downto 0)
         );
end container;

architecture Behavioral of container is
  
  signal irq : std_logic := '1';
  signal nmi : std_logic := '1';
  signal reset : std_logic := '0';
  
  signal pixelclock : std_logic;
  signal pixelclock2x : std_logic;
  signal ioclock : std_logic;
  signal clock100mhz : std_logic;

  signal fetch_port_write : fetch_port_in;
  signal fetch_port_read : fetch_port_out;

  signal fastio_rdata : unsigned(7 downto 0);
  
  signal monitor_pc : unsigned(15 downto 0);
  signal monitor_pc_drive : unsigned(15 downto 0);
  signal sseg_ca_drive : std_logic_vector(7 downto 0);
  signal sseg_an_drive : std_logic_vector(7 downto 0);
  
  signal segled_counter : unsigned(31 downto 0) := (others => '0');
  
begin
  
  dotclock1: entity work.dotclock
    port map ( clk_in1 => CLK_IN,
               clk_out1 => clock100mhz,
               clk_out2 => pixelclock,
               clk_out3 => ioclock, -- 48MHz
               PIX2CLOCK => pixelclock2x
--               clk_out3 => ioclock -- also 48MHz
               );
  
  core0: entity work.gs4502b
    port map (
      cpuclock      => pixelclock,
      ioclock       => ioclock,
      reset         => reset,

      monitor_core_id => unsigned(sw(1 downto 0)),
      monitor_pc    => monitor_pc,

      -- Fetch interface from VIC-IV to 4502's internal memory controller
      -- (which controls access to the BRAM)
      fetch_port_write => fetch_port_write,
      fetch_port_read => fetch_port_read,      

      -- CPU fast IO interface
      fastio_rdata => fastio_rdata,
      
      rom_at_8000 => '0',
      rom_at_a000 => '0',
      rom_at_c000 => '0',
      rom_at_e000 => '0',
      viciii_iomode => "11"
      );

  
  -- Hardware buttons for triggering IRQ & NMI
  irq <= not btn(0);
  nmi <= not btn(4);

  vsync <= '0';
  hsync <= '0';
  vgared <= (others => '0');
  vgagreen <= (others => '0');
  vgablue <= (others => '0');
  
  led <= (others => '0');

  -- Improve timing with drive stages
  monitor_pc_drive <= monitor_pc;
  sseg_an <= sseg_an_drive;
  sseg_ca <= sseg_ca_drive;
  
  process (pixelclock) is
    variable digit : std_logic_vector(3 downto 0);
  begin    

    if rising_edge(pixelclock) then
      segled_counter <= segled_counter + 1;

      sseg_an_drive <= (others => '1');
      sseg_an_drive(to_integer(segled_counter(19 downto 17))) <= '0';

      case segled_counter(19 downto 17) is
        when "000" =>
          digit := std_logic_vector(monitor_pc_drive(3 downto 0));
        when "001" =>
          digit := std_logic_vector(monitor_pc_drive(7 downto 4));
          -- Release reset after a while
          -- XXX Replace this little hack with proper reset logic
          reset <= '1';
        when "010" =>
          digit := std_logic_vector(monitor_pc_drive(11 downto 8));
        when "011" =>
          digit := std_logic_vector(monitor_pc_drive(15 downto 12));
        when others =>
          digit := "0000";
      end case;
      
      -- segments are:
      -- 7 - decimal point
      -- 6 - middle
      -- 5 - upper left
      -- 4 - lower left
      -- 3 - bottom
      -- 2 - lower right
      -- 1 - upper right
      -- 0 - top
      case digit is
        when x"0" => sseg_ca_drive <= "11000000";
        when x"1" => sseg_ca_drive <= "11111001";
        when x"2" => sseg_ca_drive <= "10100100";
        when x"3" => sseg_ca_drive <= "10110000";
        when x"4" => sseg_ca_drive <= "10011001";
        when x"5" => sseg_ca_drive <= "10010010";
        when x"6" => sseg_ca_drive <= "10000010";
        when x"7" => sseg_ca_drive <= "11111000";
        when x"8" => sseg_ca_drive <= "10000000";
        when x"9" => sseg_ca_drive <= "10010000";
        when x"A" => sseg_ca_drive <= "10001000";
        when x"B" => sseg_ca_drive <= "10000011";
        when x"C" => sseg_ca_drive <= "11000110";
        when x"D" => sseg_ca_drive <= "10100001";
        when x"E" => sseg_ca_drive <= "10000110";
        when x"F" => sseg_ca_drive <= "10001110";
        when others => sseg_ca_drive <= "10100001";
      end case; 
    end if;
  end process;

  
end Behavioral;
