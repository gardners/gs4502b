use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;

ENTITY gs4502b IS
  PORT (
    cpuclock : IN STD_LOGIC
    );
END gs4502b;

architecture behavioural of gs4502b is

  signal icache_lookup_line : std_logic_vector(9 downto 0);
  signal icache_read_data : std_logic_vector(71 downto 0);
  
  -- declare components here
  component ram72x1k is
    PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(71 DOWNTO 0);
    clkb : IN STD_LOGIC;
    web : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(71 DOWNTO 0)
    );
    end component;
  
begin  -- behavioural

  icacheram : ram72x1k
    port map (
      -- CPU READ interface to I-CACHE
      clka => cpuclock,
      addra => icache_lookup_line,
      douta => icache_read_data,

      -- MMU write interface to I-CACHE
      clkb => cpuclock,
      web => '0',
      addrb => (others => '0'),
      dinb => (others => '0')
      );
  
  process(cpuclock)
  begin
    if(rising_edge(cpuclock)) then 
    end if;
  end process;

end behavioural;
