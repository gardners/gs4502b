use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;

ENTITY gs4502b IS
  PORT (
    cpuclock : IN STD_LOGIC;
    monitor_PC : out unsigned(15 downto 0)
    );
END gs4502b;

architecture behavioural of gs4502b is

  signal icache_lookup_line : std_logic_vector(9 downto 0);
  signal icache_read_data : icache_line;

  signal reg_pc : unsigned(15 downto 0);

  signal expected_icache_address : unsigned(27 downto 10) := "111100001111000011";
  
  -- declare components here
  component ram72x1k is
    PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT icache_line;
    clkb : IN STD_LOGIC;
    web : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dinb : IN icache_line
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
      dinb => icache_read_data
      );
  
  process(cpuclock)
  begin
    if(rising_edge(cpuclock)) then
      monitor_pc <= reg_pc;
    end if;

    if(rising_edge(cpuclock)) then
      if expected_icache_address = icache_read_data.address then
        reg_pc <= reg_pc + 1;
      end if;


      
    end if;
  end process;

end behavioural;
