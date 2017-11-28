library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.instruction_equations.all;
use work.extra_instruction_equations.all;

package types is

  subtype translated_address is unsigned(31 downto 0);

  -- Allow upto 7 memory transactions in flight at a time
  subtype transaction_id is integer range 0 to 7;

  type instruction_bytes is record
    opcode : unsigned(7 downto 0);
    arg1 : unsigned(7 downto 0);
    arg2 : unsigned(7 downto 0);
  end record; 
  
  type addressing_mode is record
    a : boolean;
    imm8 : boolean;
    rel8 : boolean;
    rel16 : boolean;
    indirect : boolean;
    prex : boolean;
    presp : boolean;
    postx : boolean;
    posty : boolean;
    postz : boolean;
    addr8 : boolean;
    addr16 : boolean;
    imm16 : boolean;
    rel8byte3 : boolean;
  end record;
  
  subtype length_of_instruction is integer range 1 to 3;
  type ilens4 is array (0 to 3) of length_of_instruction;
  type bytes4 is array (0 to 3) of std_logic_vector(8 downto 0);  

  -- 16 byte buffer for fetching instructions from memory
  constant BYTE_BUFFER_WIDTH : integer := 16;
  type ilens is array (0 to BYTE_BUFFER_WIDTH) of integer;
  type prefetch_byte is record
    byte : std_logic_vector(8 downto 0);
    ilen : length_of_instruction;
  end record;
  type prefetch_vector is array ( 0 to 3 ) of prefetch_byte;
  type prefetch_buffer is record
    v : prefetch_vector;
    address : translated_address;
    user_flags : std_logic_vector(7 downto 0);
  end record;

  type transaction_result is record
    valid : boolean;
    id : transaction_id;
    value : unsigned(7 downto 0);
    z : boolean;
    v : boolean;
    c : boolean;
    n : boolean;    
  end record;  
  
  type fetch_port_in is record
    valid : boolean;
    translated : translated_address;
    user_flags : std_logic_vector(7 downto 0);
  end record;
  type fetch_port_out is record
    -- Announce when we can accept more input.
    ready : boolean;
    -- 4 bytes of read value
    bytes : bytes4;
    -- Address of request
    translated : translated_address;
    -- User specified flags that were presented with the request
    -- when submitted.
    user_flags : std_logic_vector(7 downto 0);
  end record;  
  type mem_port_in is record
    valid : boolean;
  end record;
  type mem_port_out is record
    result : transaction_result;
    -- Tell user if previous value has been accepted
    acknowledged : boolean;
  end record;

  type ram_interface is record
    iaddr : std_logic_vector(16 downto 0);
    maddr : std_logic_vector(16 downto 0);
    mwrite : std_logic;
    mwdata : std_logic_vector(8 downto 0);
  end record;  
  
  type alu_result is record
    value : unsigned(7 downto 0);
    c : boolean;
    n : boolean;
    z : boolean;
    v : boolean;
  end record;

  type cpu_flags is record
    c : boolean;
    d : boolean;
    i : boolean;
    z : boolean;
    e : boolean;
    v : boolean;
    n : boolean;
  end record;
  
  type cpu_registers is record
    flags : cpu_flags;
    a : unsigned(7 downto 0);
    a_dup1 : unsigned(7 downto 0);
    a_dup2 : unsigned(7 downto 0);
    a_dup3 : unsigned(7 downto 0);
    b : unsigned(7 downto 0);
    x : unsigned(7 downto 0);
    y : unsigned(7 downto 0);
    y_dup1 : unsigned(7 downto 0);
    z : unsigned(7 downto 0);
    spl : unsigned(7 downto 0);
    sph : unsigned(7 downto 0);
  end record;

  type resource_names is record
    a : transaction_id;
    b : transaction_id;
    x : transaction_id;
    y : transaction_id;
    z : transaction_id;
    spl : transaction_id;
    sph : transaction_id;
    flag_z : transaction_id;
    flag_c : transaction_id;
    flag_v : transaction_id;
    flag_n : transaction_id;
  end record;
  
  type instruction_resources is record
    a : boolean;
    b : boolean;
    x : boolean;
    y : boolean;
    z : boolean;
    spl : boolean;
    sph : boolean;
    flag_z : boolean;
    flag_n : boolean;
    flag_c : boolean;
    flag_d : boolean;
    flag_v : boolean;
  end record;

  type cpu_personality is (
    Hypervisor, CPU6502, CPU4502
    );
  
  type instruction_information is record
    -- Does this instruction load and/or store memory?
    -- (both are set for a RMW instruction)

    -- The bytes, CPU personality and address (PC and translated address)
    -- uniquely identify the instruction
    bytes : instruction_bytes;
    cpu_personality : cpu_personality;
    pc : unsigned(15 downto 0);
    translated : translated_address;

    -- Data computed from the opcode
    addressing_mode : addressing_mode;
    instruction_flags : instruction_flags;
    instruction_extra_flags : extra_instruction_flags;

    -- Address of argument
    argument_address : unsigned(15 downto 0);
    argument_translated : translated_address;

    -- Transaction ID for any indirect vector that this instruction requires
    -- (implied by addressing_mode.indirect)
    vector_fetch_transaction : unsigned(4 downto 0);
    
    -- Once we know both the opcode and the arguments, we can work out if the
    -- instruction can modify the memory map. This means looking for MAP
    -- instruction, as well as writes to $0000, $0001 (C64 ROM banking) or $D030
    -- (C65 ROM banking).  We also throw TAB into this, as it changes the
    -- behaviour of ZP addressing mode, and so we also need to flush the
    -- pipeline.
    modifies_cpu_personality : boolean;
    
    -- Address and PC information following instruction    
    pc_expected : unsigned(15 downto 0);
    pc_mispredict : unsigned(15 downto 0);

    expected_translated : translated_address;
    mispredict_translated : translated_address;

    -- Do we expect this branch to be taken?
    branch_predict : boolean;
    
  end record;
  
end package;

package body types is
end package body;
