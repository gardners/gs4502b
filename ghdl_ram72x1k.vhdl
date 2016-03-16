use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;

ENTITY ram72x1k IS
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(71 DOWNTO 0);
    clkb : IN STD_LOGIC;
    web : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(71 DOWNTO 0)
    );
END ram72x1k;

architecture behavioural of ram72x1k is

  type ram_t is array (0 to 1023) of std_logic_vector(71 downto 0);
  signal ram : ram_t;

  signal douta_drive : std_logic_vector(71 downto 0);
  signal doutb_drive : std_logic_vector(71 downto 0);
  
begin  -- behavioural

  process(clka)
  begin
    douta_drive <= ram(to_integer(unsigned(addra(9 downto 0))));
    douta <= douta_drive;

    report "I-CACHE: A Reading from $" & to_hstring(unsigned(addra))
      & " = $" & to_hstring(ram(to_integer(unsigned(addra))));
  end process;

  process (clkb,addrb,ram)
  begin
    -- doutb_drive <= ram(to_integer(unsigned(addrb(9 downto 0))));
    -- doutb <= doutb_drive;
    if(rising_edge(Clkb)) then 
      if(web='1') then
        ram(to_integer(unsigned(addrb))) <= dinb;
      end if;
    end if;
  end process;

end behavioural;
