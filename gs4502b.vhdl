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

  signal rom_from_colour_ram : std_logic := '0';

  -- General pipeline control
  signal instruction_ready : std_logic := '0';
  signal expected_instruction_address : unsigned(31 downto 0)
    := "00001111000011110000111010101010";
  signal expected_instruction_pch : unsigned(15 downto 8) := "00000000";

  -- Inputs to first stage of pipeline (decode stage)
  signal icache_lookup_line : std_logic_vector(9 downto 0);
  signal icache_read_data : std_logic_vector(105 downto 0);
  signal icache_write_data : std_logic_vector(105 downto 0) := (others => '1');
  signal cpu_divert : std_logic := '0';
  signal cpu_divert_line : unsigned(9 downto 0);

  -- Signals output by decode stage
  signal stage_decode_instruction_address : unsigned(31 downto 0);
  signal stage_decode_instruction_bytes : std_logic_vector(23 downto 0);
  signal stage_decode_pc_expected_translated : unsigned(31 downto 0);
  signal stage_decode_pc_mispredict_translated : unsigned(31 downto 0);
  signal stage_decode_pch : unsigned(15 downto 8);
  signal stage_decode_branch_predict : std_logic;
  signal icache_ram_read_address : std_logic_vector(9 downto 0);
  signal stage_decode_resources_required : instruction_resources;
  signal stage_decode_resources_modified : instruction_resources;
  signal stage_decode_instruction_information : instruction_information;

  -- Signals output by validate stage
  signal validate_stall : std_logic;
  signal stage_validate_instruction_address : unsigned(31 downto 0);
  signal stage_validate_instruction_bytes : std_logic_vector(23 downto 0);
  signal stage_validate_pc_expected_translated : unsigned(31 downto 0);
  signal stage_validate_pc_mispredict_translated : unsigned(31 downto 0);
  signal stage_validate_pch : unsigned(15 downto 8);
  signal stage_validate_branch_predict : std_logic;
  signal stage_validate_resources_required : instruction_resources;
  signal stage_validate_resources_modified : instruction_resources;
  signal stage_validate_instruction_information : instruction_information;
  signal instruction_valid : std_logic;

  -- Signals output by execute stage
  signal execute_stall : std_logic;
  signal stage_execute_resources_locked : instruction_resources := (others => false);
  signal stage_execute_transaction_id : transaction_id;
  signal stage_execute_transaction_valid : boolean := false;
  
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
  
begin  -- behavioural
  
  icacheram : entity work.icache_ram
    port map (
      -- CPU READ interface to I-CACHE
      clkb => cpuclock,
      addrb => icache_ram_read_address,
      doutb => icache_read_data,

      -- MMU write interface to I-CACHE
      clka => cpuclock,
      wea => "0",
      addra => (others => '0'),
      dina => icache_write_data
      );

  decode_stage: entity work.gs4502b_stage_decode
    port map (
      cpuclock => cpuclock,
      cpuport_value => cpuport_value,
      cpuport_ddr => cpuport_ddr,
      viciii_iomode => viciii_iomode,
      reg_map_low => reg_map_low,
      reg_mb_low => reg_mb_low,
      reg_offset_low => reg_offset_low,
      reg_map_high => reg_map_high,
      reg_mb_high => reg_mb_high,
      reg_offset_high => reg_offset_high,
      rom_at_c000 => rom_at_c000,
      rom_at_e000 => rom_at_e000,
      rom_at_8000 => rom_at_8000,
      rom_at_a000 => rom_at_a000,

      -- The fields must match those specified in icachetypes.vhdl
      icache_src_address_in => unsigned(icache_read_data(21 downto 0)),
      icache_bytes_in => icache_read_data(45 downto 22),
      pch_in => unsigned(icache_read_data(85 downto 78)),
      pc_expected => unsigned(icache_read_data(61 downto 46)),
      pc_mispredict => unsigned(icache_read_data(77 downto 62)),
      branch_predict_in => icache_read_data(86),
      cpu_divert => cpu_divert,
      cpu_divert_line => cpu_divert_line,
      stall => validate_stall,
      icache_src_address_out => stage_decode_instruction_address,
      icache_bytes_out => stage_decode_instruction_bytes,
      pch_out => stage_decode_pch,
      pc_expected_translated => stage_decode_pc_expected_translated,      
      pc_mispredict_translated => stage_decode_pc_mispredict_translated,
      branch_predict_out => stage_decode_branch_predict,
      std_logic_vector(next_cache_line) => icache_ram_read_address
      );

  validate_stage: entity work.gs4502b_stage_validate
    port map (
      cpuclock => cpuclock,

      stall_in => execute_stall,
      resources_freshly_locked_by_execute_stage
      => stage_execute_resources_locked,
      resource_lock_transaction_id_in => stage_execute_transaction_id,
      resource_lock_transaction_valid_in => stage_execute_transaction_valid,
      
      instruction_address_in => stage_decode_instruction_address,
      instruction_bytes_in => stage_decode_instruction_bytes,
      pch_in => stage_decode_pch,
      pc_expected_in => stage_decode_pc_expected_translated,
      pc_mispredict_in => stage_decode_pc_mispredict_translated,
      branch_predict_in => stage_decode_branch_predict,
      resources_required_in => stage_decode_resources_required,
      resources_modified_in => stage_decode_resources_modified,
      instruction_information_in => stage_decode_instruction_information,

      instruction_address_out => stage_validate_instruction_address,
      instruction_bytes_out => stage_validate_instruction_bytes,
      pch_out => stage_validate_pch,
      pc_expected_translated => stage_validate_pc_expected_translated,
      pc_mispredict_translated => stage_validate_pc_mispredict_translated,
      branch_predict_out => stage_validate_branch_predict,
      instruction_valid => instruction_valid,      
      resources_required_out => stage_validate_resources_required,
      resources_modified_out => stage_validate_resources_modified,
      instruction_information_out => stage_validate_instruction_information,
      stall_out => validate_stall

      );
  
  process(cpuclock, icache_read_data)
    variable icache_bits : icache_line;
  begin
    if(rising_edge(cpuclock)) then
      monitor_pc <= reg_pc;
    end if;

    if(rising_edge(cpuclock)) then
            
      if (expected_instruction_address = stage_decode_instruction_address)
         and (expected_instruction_pch = stage_decode_pch) then
        instruction_ready <= '1';
      else
        instruction_ready <= '0';
      end if;

      -- But do what calculations we can on what we read from the cache line
      -- immediately.     
      
      -- XXX Check for branch conditions by checking if instruction is
      -- branch, and if any of the branch conditions fail, so that we know
      -- whether to take the branch address or not.



      -- Do we have the next instruction we were looking for?
      if instruction_ready='1' then
        -- Yes, execute instruction

        -- XXX Note implemented. For now we just keep requesting a given address
        expected_instruction_address <= x"07FFFFFF";
        expected_instruction_pch(15 downto 8) <= x"FF";
        
      end if;

      -- Can we retire any resource blocks?
      -- i.e., load registers from memory, or set flags based on conclusion of
      -- an instruction, or return of RMW result flags from MMU.

      
    end if;
  end process;

end behavioural;
