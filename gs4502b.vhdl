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

  -- Instruction cache interface and logic
  signal icache_lookup_line : std_logic_vector(9 downto 0);
  signal icache_read_data : std_logic_vector(71 downto 0);
  signal icache_read_data_drive : std_logic_vector(71 downto 0);
  signal icache_ready : std_logic := '0';
  signal expected_icache_address : unsigned(27 downto 0) := "1111000011110000111010101010";
  signal icache_read_line_number : unsigned(9 downto 0);
  signal icache_read_line_number_drive : unsigned(9 downto 0);
  signal icache_read_line_number_drive2 : unsigned(9 downto 0);

  -- CPU Registers & Flags
  signal reg_pc : unsigned(15 downto 0);
  signal reg_a : unsigned(7 downto 0);
  signal reg_x : unsigned(7 downto 0);
  signal reg_y : unsigned(7 downto 0);
  signal reg_z : unsigned(7 downto 0);
  signal reg_spl : unsigned(7 downto 0);
  signal reg_sph : unsigned(7 downto 0);



  
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
      doutb => icache_read_data_drive,

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
      icache_read_data <= icache_read_data_drive;
      -- Also drive through the cache line ID, so that we know the low-order bits
      -- of what is being presented.
      icache_read_line_number <= icache_read_line_number_drive;
      icache_read_line_number_drive <= icache_read_line_number_drive2;
      
      -- But do what calculations we can on what we read from the cache line
      -- immediately.
      if (std_logic_vector(expected_icache_address(27 downto 10))
          = icache_read_data_drive(17 downto 0))
        and (icache_read_line_number = expected_icache_address(9 downto 0))
      then
        icache_ready <= '1';
      else
        icache_ready <= '0';
      end if;
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
        reg_pc <= reg_pc + 1;
      end if;

      -- Can we retire any resource blocks?
      -- i.e., load registers from memory, or set flags based on conclusion of
      -- an instruction, or return of RMW result flags from MMU.

      
    end if;
  end process;

end behavioural;
