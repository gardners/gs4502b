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
    wea : IN STD_LOGIC_VECTOR(0 downto 0);
    addra : IN std_logic_vector(9 DOWNTO 0);
    dina : IN std_logic_vector(107 downto 0);
    douta : OUT std_logic_vector(107 downto 0);
    clkb : IN STD_LOGIC;
    web : in STD_LOGIC_VECTOR(0 downto 0);
    dinb : IN std_logic_vector(107 downto 0);
    addrb : IN std_logic_vector(9 DOWNTO 0);
    doutb : OUT std_logic_vector(107 downto 0)
    );
END icache_ram;

architecture behavioural of icache_ram is

  type ram_t is array (0 to 1023) of std_logic_vector(107 downto 0);
  signal ram : ram_t;
  signal doutb_drive : std_logic_vector(107 downto 0);
  signal douta_drive : std_logic_vector(107 downto 0);
  
begin  -- behavioural

  process(clka,addra,ram)
  begin
    if(rising_edge(Clka)) then
      -- Mimic the one cycle read latency of the coregen generated RAM
      doutb <= doutb_drive;
      doutb_drive <= ram(to_integer(unsigned(addrb)));
      douta <= douta_drive;
      douta_drive <= ram(to_integer(unsigned(addra)));
    end if;
    
    if(rising_edge(Clka)) then 
      if(wea(0)='1') then
        ram(to_integer(unsigned(addra(9 downto 0)))) <= dina;
        report "I-CACHE: A writing to $" & to_hstring(unsigned(addra))
          & " = $" & to_hstring(dina);
      else
        report "I-CACHE: A Reading from $" & to_hstring(unsigned(addra));
      end if;
    end if;
    if(rising_edge(Clkb)) then 
      if(web(0)='1') then
        ram(to_integer(unsigned(addrb(9 downto 0)))) <= dinb;
        report "I-CACHE: B writing to $" & to_hstring(unsigned(addrb))
          & " = $" & to_hstring(dinb);
      else
        report "I-CACHE: B Reading from $" & to_hstring(unsigned(addrb));
      end if;
    end if;

  end process;

end behavioural;
