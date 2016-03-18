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
    monitor_PC : out unsigned(15 downto 0);

    rom_at_8000 : in std_logic;
    rom_at_a000 : in std_logic;
    rom_at_c000 : in std_logic;
    rom_at_e000 : in std_logic;
    viciii_iomode : in std_logic_vector(1 downto 0)
    );
END gs4502b;

architecture behavioural of gs4502b is

  -- Instruction cache interface and logic
  signal icache_lookup_line : std_logic_vector(9 downto 0);
  signal icache_read_data : std_logic_vector(105 downto 0);
  signal icache_read_data_drive : std_logic_vector(105 downto 0);
  signal icache_read_data_drive2 : std_logic_vector(105 downto 0);
  signal icache_ready : std_logic := '0';
  signal expected_icache_address : unsigned(31 downto 0) := "00001111000011110000111010101010";
  signal icache_next_pc_resolved : unsigned(31 downto 0);
  signal icache_branch_pc_resolved : unsigned(31 downto 0);

  signal address_translator0_address_in : unsigned(15 downto 0);
  signal address_translator1_address_in : unsigned(15 downto 0);
  signal address_translator0_address_out : unsigned(31 downto 0);
  signal address_translator1_address_out : unsigned(31 downto 0);
  

  -- CPU Registers & Flags
  signal reg_pc : unsigned(15 downto 0);
  signal reg_a : unsigned(7 downto 0);
  signal reg_x : unsigned(7 downto 0);
  signal reg_y : unsigned(7 downto 0);
  signal reg_z : unsigned(7 downto 0);
  signal reg_spl : unsigned(7 downto 0);
  signal reg_sph : unsigned(7 downto 0);

  -- Memory mapping registers and derivatives
  signal reg_mb_low : unsigned(11 downto 0);
  signal reg_offset_low : unsigned(11 downto 0);
  signal reg_map_low : std_logic_vector(3 downto 0);
  signal reg_mb_high : unsigned(11 downto 0);
  signal reg_map_high : std_logic_vector(3 downto 0);
  signal reg_offset_high : unsigned(11 downto 0);
  signal cpuport_value : std_logic_vector(2 downto 0);
  signal cpuport_ddr : std_logic_vector(2 downto 0);

  component address_translator IS
    PORT (
      cpuclock : IN STD_LOGIC;

      -- Things that affect address mapping
      cpuport_value: in std_logic_vector(2 downto 0);
      cpuport_ddr: in std_logic_vector(2 downto 0);
      viciii_iomode : in std_logic_vector(1 downto 0);
      rom_from_colour_ram : in std_logic;
      reg_map_low : in std_logic_vector(3 downto 0);
      reg_mb_low : in unsigned(11 downto 0);
      reg_offset_low : in unsigned(11 downto 0);
      reg_map_high : in std_logic_vector(3 downto 0);
      reg_mb_high : in unsigned(11 downto 0);
      reg_offset_high : in unsigned(11 downto 0);
      rom_at_c000 : in std_logic;
      rom_at_e000 : in std_logic;
      rom_at_a000 : in std_logic;
      rom_at_8000 : in std_logic;

      memory_map_has_changed : out std_logic := '0';
    
      address_in : in unsigned(15 downto 0);
      read_address : out unsigned(31 downto 0);
      write_address : out unsigned(31 downto 0)
      );
  END component;

  
  component icache_ram IS
  PORT (
      clka : IN STD_LOGIC;
      wea : IN STD_LOGIC_vector(0 downto 0);
      addra : IN std_logic_vector(9 DOWNTO 0);
      dina : IN std_logic_vector(105 downto 0);
      clkb : IN STD_LOGIC;
      addrb : IN std_logic_vector(9 DOWNTO 0);
      doutb : OUT std_logic_vector(105 downto 0)
      );
    END component;

  
begin  -- behavioural

  address_translator0: address_translator
    port map (
      cpuclock => cpuclock,
      cpuport_value => cpuport_value,
      cpuport_ddr => cpuport_ddr,
      viciii_iomode => viciii_iomode,
      rom_from_colour_ram => '0',
      reg_map_low => reg_map_low,
      reg_mb_low => reg_mb_low,
      reg_offset_low => reg_offset_low,
      reg_map_high => reg_map_high,
      reg_mb_high => reg_mb_high,
      reg_offset_high => reg_offset_high,
      rom_at_8000 => rom_at_8000,
      rom_at_a000 => rom_at_a000,
      rom_at_c000 => rom_at_c000,
      rom_at_e000 => rom_at_e000,

      address_in => address_translator0_address_in,
      read_address => address_translator0_address_out
      );
  
  address_translator1: address_translator
    port map (
      cpuclock => cpuclock,
      cpuport_value => cpuport_value,
      cpuport_ddr => cpuport_ddr,
      viciii_iomode => viciii_iomode,
      rom_from_colour_ram => '0',
      reg_map_low => reg_map_low,
      reg_mb_low => reg_mb_low,
      reg_offset_low => reg_offset_low,
      reg_map_high => reg_map_high,
      reg_mb_high => reg_mb_high,
      reg_offset_high => reg_offset_high,
      rom_at_8000 => rom_at_8000,
      rom_at_a000 => rom_at_a000,
      rom_at_c000 => rom_at_c000,
      rom_at_e000 => rom_at_e000,

      address_in => address_translator1_address_in,
      read_address => address_translator1_address_out
      );
  
  icacheram : icache_ram
    port map (
      -- CPU READ interface to I-CACHE
      clkb => cpuclock,
      addrb => icache_lookup_line,
      doutb => icache_read_data_drive2,

      -- MMU write interface to I-CACHE
      clka => cpuclock,
      wea => "0",
      addra => (others => '0'),
      dina => icache_read_data
      );


  
  process(cpuclock, icache_read_data)
    variable icache_bits : icache_line;
  begin
    if(rising_edge(cpuclock)) then
      monitor_pc <= reg_pc;
    end if;

    if(rising_edge(cpuclock)) then
      -- Drive signals to help keep logic shallow to allow pipelining to work

      -- Pipeline Stage 1: Drive, and resolve next_pc and branch_pc
      icache_bits := std_logic_vector_to_icache_line(icache_read_data_drive2);
      icache_read_data_drive <= icache_read_data_drive2;
      address_translator0_address_in <= icache_bits.next_pc;
      address_translator1_address_in <= icache_bits.branch_pc;

      -- Pipeline Stage 2: Get resolved addresses out, report cache misses to MMU
      icache_read_data <= icache_read_data_drive;
      icache_next_pc_resolved <= address_translator0_address_out;
      icache_branch_pc_resolved <= address_translator1_address_out;
      -- If read cache line is correct, but upper bits are wrong, then we have
      -- a cache miss: Tell MMU so that it can fetch it.
      
      -- Is the next instruction the one we are looking for?
      -- XXX Need to know if the branch is taken
      icache_bits := std_logic_vector_to_icache_line(icache_read_data_drive);
      if expected_icache_address = icache_bits.address then
        icache_ready <= '1';
      else
        icache_ready <= '0';
      end if;

      -- But do what calculations we can on what we read from the cache line
      -- immediately.

      
      -- Ask for next instruction automatically.  This get changed later only if
      -- a branch is taken, or something else causes us to ask for something different.
      -- The idea is that we keep the instruction cache pipeline as full as possible,
      -- so that we can dispatch one instruction per cycle.
      -- We need to think about how we can stall the pipeline if an instruction
      -- takes more than 1 cycle to retire, or whether it just isn't worth the
      -- effort. My gut feeling is that it isn't worth it. Instructions will tend
      -- to take only a single cycle in this part of the CPU, until the CPU blocks
      -- waiting for another instruction to be retired by the MMU.  At which point
      -- we can think about whether we need some funny logic to keep the instruction
      -- cache presenting the correct instruction at that point.
      icache_bits := std_logic_vector_to_icache_line(icache_read_data_drive);
      icache_lookup_line
        <= std_logic_vector(icache_bits.next_pc(9 downto 0));

      
      -- XXX Check for branch conditions by checking if instruction is
      -- branch, and if any of the branch conditions fail, so that we know
      -- whether to take the branch address or not.



      -- Do we have the next instruction we were looking for?
      if icache_ready='1' then
        -- Yes, execute instruction
        icache_bits := std_logic_vector_to_icache_line(icache_read_data);

        -- Update PC based on instruction
        reg_pc <= icache_bits.next_pc;
        -- And then which address we expect to see from the cache.
        -- (We don't need to update the line we are asking for from the cache,
        -- however, because for us to have read this from the cache, the next
        -- instruction will be automatically asked to come out next.  We only
        -- need to change it if we are taking a branch or triggering an interrupt.
        -- For branches, we can just take the appropriate bits from the decoded
        -- instruction.  For interrupts, it would be nice to cache the vectors,
        -- but that isn't entirely easy, since memory mappings can affect them.
        expected_icache_address(15 downto 0) <= icache_bits.next_pc;
        -- The other fun bit is that we need to work out the upper bits we are
        -- requesting. For this, we need a little address resolution cache that
        -- we can look in. If the appropriate page of memory is in the slot, then
        -- we can set the upper bits. Else we need to hold this instruction.

        -- XXX Address should be resolved in a specific pipeline stage.
        -- This means no address resolution cache etc :)
        expected_icache_address(31 downto 8) <= x"07FFFF";
        
      end if;

      -- Can we retire any resource blocks?
      -- i.e., load registers from memory, or set flags based on conclusion of
      -- an instruction, or return of RMW result flags from MMU.

      
    end if;
  end process;

end behavioural;
