use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;

ENTITY ram72x1k IS
  PORT (
    clka : IN STD_LOGIC;
    addra : IN unsigned(9 DOWNTO 0);
    douta : OUT std_logic_vector(71 downto 0);
    ena : in std_logic;
    wea : in std_logic;
    dina : in std_logic_vector(71 downto 0);
    clkb : IN STD_LOGIC;
    web : IN STD_LOGIC;
    addrb : IN unsigned(9 DOWNTO 0);
    dinb : IN std_logic_vector(71 downto 0)
    );
END ram72x1k;

architecture behavioural of ram72x1k is

  type ram_t is array (0 to 1023) of std_logic_vector(71 downto 0);
  signal ram : ram_t;

  signal douta_drive : std_logic_vector(71 downto 0);
  
begin  -- behavioural

  process(clka,addra,ram)
  begin
    douta <= douta_drive;
    douta_drive <= ram(to_integer(addra));
    
    report "I-CACHE: A Reading from $" & to_hstring(unsigned(addra));
--      & " = $" & to_hstring(ram(to_integer(unsigned(addra))));

    if(rising_edge(Clka)) then 
      if ena='1' then
        if(wea='1') then
          ram(to_integer(unsigned(addra(9 downto 0)))) <= dina;
          report "COLOURRAM: A writing to $" & to_hstring(unsigned(addra))
            & " = $" & to_hstring(dina);
        end if;
      end if;
    end if;

  end process;

  process (clkb,addrb,ram)
  begin
    if(rising_edge(Clkb)) then 
      if(web='1') then
        ram(to_integer(addrb)) <= dinb;
      end if;
    end if;
  end process;

end behavioural;
