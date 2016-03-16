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

  component dotclock is
    port (
      CLK_IN1           : in     std_logic;
    -- Clock out ports
    CLK_OUT1          : out    std_logic;
    CLK_OUT2          : out    std_logic;
    CLK_OUT3          : out    std_logic;
    CPUCLOCK          : out    std_logic;
--    IOCLOCK          : out    std_logic;
    PIX2CLOCK          : out    std_logic
      );
  end component;
    
  signal irq : std_logic := '1';
  signal nmi : std_logic := '1';
  
  signal pixelclock : std_logic;
  signal pixelclock2x : std_logic;
  signal cpuclock : std_logic;
--  signal ioclock : std_logic;
  
  signal segled_counter : unsigned(31 downto 0) := (others => '0');
  
begin
  
  dotclock1: component dotclock
    port map ( clk_in1 => CLK_IN,
               clk_out1 => clock100mhz,
               clk_out2 => pixelclock,
               clk_out3 => cpuclock, -- 48MHz
               PIX2CLOCK => pixelclock2x
--               clk_out3 => ioclock -- also 48MHz
               );
  
  core0: gs4502b
    port map (
      cpuclock      => pixelclock,
      );

  
  -- Hardware buttons for triggering IRQ & NMI
  irq <= not btn(0);
  nmi <= not btn(4);
  
end Behavioral;
