use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;

package address_translator is
    function resolve_address_to_long(short_address : unsigned(15 downto 0);
                                   writeP : boolean;

                                   -- Things that affect address mapping
                                   cpuport_value: in std_logic_vector(2 downto 0);
                                   cpuport_ddr: in std_logic_vector(2 downto 0);
                                   viciii_iomode : in std_logic_vector(1 downto 0);
                                   reg_map_low : in std_logic_vector(3 downto 0);
                                   reg_mb_low : in unsigned(11 downto 0);
                                   reg_offset_low : in unsigned(11 downto 0);
                                   reg_map_high : in std_logic_vector(3 downto 0);
                                   reg_mb_high : in unsigned(11 downto 0);
                                   reg_offset_high : in unsigned(11 downto 0);
                                   rom_at_8000 : in std_logic;
                                   rom_at_a000 : in std_logic;
                                   rom_at_c000 : in std_logic;
                                   rom_at_e000 : in std_logic

                                   ) return unsigned;
end package;

package body address_translator is
  
  function resolve_address_to_long(short_address : unsigned(15 downto 0);
                                   writeP : boolean;

                                   -- Things that affect address mapping
                                   cpuport_value: in std_logic_vector(2 downto 0);
                                   cpuport_ddr: in std_logic_vector(2 downto 0);
                                   viciii_iomode : in std_logic_vector(1 downto 0);
                                   reg_map_low : in std_logic_vector(3 downto 0);
                                   reg_mb_low : in unsigned(11 downto 0);
                                   reg_offset_low : in unsigned(11 downto 0);
                                   reg_map_high : in std_logic_vector(3 downto 0);
                                   reg_mb_high : in unsigned(11 downto 0);
                                   reg_offset_high : in unsigned(11 downto 0);
                                   rom_at_8000 : in std_logic;
                                   rom_at_a000 : in std_logic;
                                   rom_at_c000 : in std_logic;
                                   rom_at_e000 : in std_logic

                                   )
      return unsigned is 
      variable temp_address : translated_address;
      variable blocknum : integer;
      variable lhc : std_logic_vector(2 downto 0);
    begin  -- resolve_long_address
      
      -- Now apply C64-style $01 lines first, because MAP and $D030 take precedence
      blocknum := to_integer(short_address(15 downto 12));

      lhc := std_logic_vector(cpuport_value(2 downto 0));
      lhc(2) := lhc(2) or (not cpuport_ddr(2));
      lhc(1) := lhc(1) or (not cpuport_ddr(1));
      lhc(0) := lhc(0) or (not cpuport_ddr(0));
      
      -- Examination of the C65 interface ROM reveals that MAP instruction
      -- takes precedence over $01 CPU port when MAP bit is set for a block of RAM.

      -- From https://groups.google.com/forum/#!topic/comp.sys.cbm/C9uWjgleTgc
      -- Port pin (bit)    $A000 to $BFFF       $D000 to $DFFF       $E000 to $FFFF
      -- 2 1 0             Read       Write     Read       Write     Read       Write
      -- --------------    ----------------     ----------------     ----------------
      -- 0 0 0             RAM        RAM       RAM        RAM       RAM        RAM
      -- 0 0 1             RAM        RAM       CHAR-ROM   RAM       RAM        RAM
      -- 0 1 0             RAM        RAM       CHAR-ROM   RAM       KERNAL-ROM RAM
      -- 0 1 1             BASIC-ROM  RAM       CHAR-ROM   RAM       KERNAL-ROM RAM
      -- 1 0 0             RAM        RAM       RAM        RAM       RAM        RAM
      -- 1 0 1             RAM        RAM       I/O        I/O       RAM        RAM
      -- 1 1 0             RAM        RAM       I/O        I/O       KERNAL-ROM RAM
      -- 1 1 1             BASIC-ROM  RAM       I/O        I/O       KERNAL-ROM RAM
      
      -- default is address in = address out
      temp_address(31 downto 16) := (others => '0');
      temp_address(15 downto 0) := short_address;

      -- IO
      if (blocknum=13) then
        temp_address(11 downto 0) := short_address(11 downto 0);
        if writeP then
          case lhc(2 downto 0) is
            when "000" => temp_address(31 downto 12) := x"0000D";  -- WRITE RAM
            when "001" => temp_address(31 downto 12) := x"0000D";  -- WRITE RAM
            when "010" => temp_address(31 downto 12) := x"0000D";  -- WRITE RAM
            when "011" => temp_address(31 downto 12) := x"0000D";  -- WRITE RAM
            when "100" => temp_address(31 downto 12) := x"0000D";  -- WRITE RAM
            when others =>
              -- All else accesses IO
              -- C64/C65/C65GS I/O is based on which secret knock has been applied
              -- to $D02F
              temp_address(31 downto 12) := x"0FFD3";
              temp_address(13 downto 12) := unsigned(viciii_iomode);          
          end case;        
        else
          -- READING
          case lhc(2 downto 0) is
            when "000" => temp_address(31 downto 12) := x"0000D";  -- READ RAM
            when "001" => temp_address(31 downto 12) := x"0002D";  -- CHARROM
            when "010" => temp_address(31 downto 12) := x"0002D";  -- CHARROM
            when "011" => temp_address(31 downto 12) := x"0002D";  -- CHARROM
            when "100" => temp_address(31 downto 12) := x"0000D";  -- READ RAM
            when others =>
              -- All else accesses IO
              -- C64/C65/C65GS I/O is based on which secret knock has been applied
              -- to $D02F
              temp_address(31 downto 12) := x"0FFD3";
              temp_address(13 downto 12) := unsigned(viciii_iomode);          
          end case;              end if;
      end if;

      -- C64 KERNEL
      if reg_map_high(3)='0' then
        if (blocknum=14) and (lhc(1)='1') and (writeP=false) then
          temp_address(31 downto 12) := x"0002E";      
        end if;
        if (blocknum=15) and (lhc(1)='1') and (writeP=false) then
          temp_address(31 downto 12) := x"0002F";
        end if;
      end if;
      -- C64 BASIC
      if reg_map_high(1)='0' then
        if (blocknum=10) and (lhc(0)='1') and (lhc(1)='1') and (writeP=false) then
          temp_address(31 downto 12) := x"0002A";
        end if;
        if (blocknum=11) and (lhc(0)='1') and (lhc(1)='1') and (writeP=false) then
          temp_address(31 downto 12) := x"0002B";      
        end if;
      end if;

      -- Lower 8 address bits are never changed
      temp_address(7 downto 0):=short_address(7 downto 0);

      -- Add the map offset if required
      blocknum := to_integer(short_address(14 downto 13));
      if short_address(15)='1' then
        if reg_map_high(blocknum)='1' then
          temp_address(31 downto 20) := reg_mb_high;
          temp_address(19 downto 8) := reg_offset_high+to_integer(short_address(15 downto 8));
          temp_address(7 downto 0) := short_address(7 downto 0);       
        end if;
      else
        if reg_map_low(blocknum)='1' then
          temp_address(31 downto 20) := reg_mb_low;
          temp_address(19 downto 8) := reg_offset_low+to_integer(short_address(15 downto 8));
          temp_address(7 downto 0) := short_address(7 downto 0);
          report "mapped memory address is $" & to_hstring(temp_address) severity note;
        end if;
      end if;
      
      -- $D030 ROM select lines:
      blocknum := to_integer(short_address(15 downto 12));
      if (blocknum=14 or blocknum=15) and rom_at_e000='1' then
        temp_address(31 downto 12) := x"0003E";
        if blocknum=15 then temp_address(12):='1'; end if;
      end if;
      if (blocknum=12) and rom_at_c000='1' then
        temp_address(31 downto 12) := x"0002C";
      end if;
      if (blocknum=10 or blocknum=11) and rom_at_a000='1' then
        temp_address(31 downto 12) := x"0003A";
        if blocknum=11 then temp_address(12):='1'; end if;
      end if;
      if (blocknum=9) and rom_at_8000='1' then
        temp_address(31 downto 12) := x"00039";
      end if;
      if (blocknum=8) and rom_at_8000='1' then
        temp_address(31 downto 12) := x"00038";
      end if;
            
      return temp_address;
    end resolve_address_to_long;

end package body;

--ENTITY address_translator IS
--  PORT (
--    cpuclock : IN STD_LOGIC;

--    -- Things that affect address mapping
--    cpuport_value: in std_logic_vector(2 downto 0);
--    cpuport_ddr: in std_logic_vector(2 downto 0);
--    viciii_iomode : in std_logic_vector(1 downto 0);
--    rom_from_colour_ram : in std_logic;
--    reg_map_low : in std_logic_vector(3 downto 0);
--    reg_mb_low : in unsigned(11 downto 0);
--    reg_offset_low : in unsigned(11 downto 0);
--    reg_map_high : in std_logic_vector(3 downto 0);
--    reg_mb_high : in unsigned(11 downto 0);
--    reg_offset_high : in unsigned(11 downto 0);
--    rom_at_8000 : in std_logic;
--    rom_at_a000 : in std_logic;
--    rom_at_c000 : in std_logic;
--    rom_at_e000 : in std_logic;

--    memory_map_has_changed : out std_logic := '0';
    
--    address_in : in unsigned(15 downto 0);
--    read_address : out translated_address;
--    write_address : out translated_address
--    );
--END address_translator;
--
--
--architecture behavioural of address_translator is

--    signal last_cpuport_value: std_logic_vector(2 downto 0);
--    signal last_rom_from_colour_ram : std_logic;
--    signal last_reg_map_low : std_logic_vector(3 downto 0);
--    signal last_reg_mb_low : unsigned(11 downto 0);
--    signal last_reg_offset_low : unsigned(11 downto 0);
--    signal last_reg_map_high : std_logic_vector(3 downto 0);
--    signal last_reg_mb_high : unsigned(11 downto 0);
--    signal last_reg_offset_high : unsigned(11 downto 0);
--    signal last_rom_at_c000 : std_logic;
--    signal last_rom_at_e000 : std_logic;
--    signal last_rom_at_a000 : std_logic;
--    signal last_rom_at_8000 : std_logic;
      
--begin

--  process(cpuclock,address_in,last_cpuport_value,rom_at_8000,rom_at_a000,
--          rom_at_c000,rom_at_e000,rom_from_colour_ram,cpuport_value,
--          cpuport_ddr,reg_map_low,reg_mb_low,reg_offset_low,reg_map_high,
--          reg_mb_high,reg_offset_high)
--  begin

--    if rising_edge(cpuclock) then
--      -- Tell CPU if memory map has changed
--      if (last_cpuport_value /= cpuport_value)
--        or (last_rom_from_colour_ram /= rom_from_colour_ram)
--        or (last_reg_map_low /= reg_map_low)
--        or (last_reg_mb_low /= reg_mb_low)
--        or (last_reg_offset_low /= reg_offset_low)
--        or (last_reg_map_high /= reg_map_high)
--        or (last_reg_mb_high /= reg_mb_high)
--        or (last_reg_offset_high /= reg_offset_high)
--        or (last_rom_at_8000 /= rom_at_8000) 
--        or (last_rom_at_a000 /= rom_at_a000) 
--        or (last_rom_at_c000 /= rom_at_c000) 
--        or (last_rom_at_e000 /= rom_at_e000) then
--        memory_map_has_changed <= '1';
--      else
--        memory_map_has_changed <= '0';
--      end if;
--      last_cpuport_value <= cpuport_value;
--      last_rom_from_colour_ram <= rom_from_colour_ram;
--      last_reg_map_low <= reg_map_low;
--      last_reg_mb_low <= reg_mb_low;
--      last_reg_offset_low <= reg_offset_low;
--      last_reg_map_high <= reg_map_high;
--      last_reg_mb_high <= reg_mb_high;
--      last_reg_offset_high <= reg_offset_high;
--      last_rom_at_8000 <= rom_at_8000; 
--      last_rom_at_a000 <= rom_at_a000; 
--      last_rom_at_c000 <= rom_at_c000; 
--      last_rom_at_e000 <= rom_at_e000;                                         
--    end if;
--end process;

--end behavioural;


