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
  signal icache_read_data : std_logic_vector(71 downto 0);

  signal reg_pc : unsigned(15 downto 0);

  signal expected_icache_address : unsigned(27 downto 10) := "111100001111000011";
  
  component icache_ram IS
  PORT (
      clka : IN STD_LOGIC;
      wea : IN STD_LOGIC_vector(0 downto 0);
      addra : IN std_logic_vector(9 DOWNTO 0);
      dina : IN std_logic_vector(71 downto 0);
      clkb : IN STD_LOGIC;
      addrb : IN std_logic_vector(9 DOWNTO 0);
      doutb : OUT std_logic_vector(71 downto 0)
      );
    END component;

  
begin  -- behavioural

  icacheram : icache_ram
    port map (
      -- CPU READ interface to I-CACHE
      clkb => cpuclock,
      addrb => icache_lookup_line,
      doutb => icache_read_data,

      -- MMU write interface to I-CACHE
      clka => cpuclock,
      wea => "0",
      addra => (others => '0'),
      dina => icache_read_data
      );
  
  process(cpuclock, icache_read_data)
  begin
    if(rising_edge(cpuclock)) then
      monitor_pc <= reg_pc;
    end if;

    if(rising_edge(cpuclock)) then
      icache_lookup_line(9 downto 0) <= std_logic_vector(reg_pc(9 downto 0));
      if std_logic_vector(expected_icache_address)
        = icache_read_data(17 downto 0) then
        reg_pc <= reg_pc + 1;
      end if;


      
    end if;
  end process;

end behavioural;
