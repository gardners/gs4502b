use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;

ENTITY icache_ram IS
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC;
    addra : IN std_logic_vector(9 DOWNTO 0);
    dina : IN std_logic_vector(71 downto 0);
    clkb : IN STD_LOGIC;
    addrb : IN std_logic_vector(9 DOWNTO 0);
    doutb : OUT std_logic_vector(71 downto 0)
    );
END icache_ram;

architecture behavioural of icache_ram is

  type ram_t is array (0 to 1023) of std_logic_vector(71 downto 0);
  signal ram : ram_t;
  
begin  -- behavioural

  process(clka,addra,ram)
  begin
    if(rising_edge(Clka)) then 
      doutb <= ram(to_integer(unsigned(addrb)));
    end if;
    
    report "I-CACHE: A Reading from $" & to_hstring(unsigned(addra));
--      & " = $" & to_hstring(ram(to_integer(unsigned(addra))));

    if(rising_edge(Clka)) then 
      if(wea='1') then
        ram(to_integer(unsigned(addra(9 downto 0)))) <= dina;
        report "COLOURRAM: A writing to $" & to_hstring(unsigned(addra))
          & " = $" & to_hstring(dina);
      end if;
    end if;

  end process;

end behavioural;
