library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;

package types is

  type something_useless is record
    an : integer;
  end record;
  
  -- Allow upto 7 memory transactions in flight at a time
  subtype transaction_id is integer range 0 to 7;

  type instruction_bytes is record
    opcode : unsigned(7 downto 0);
    arg1 : unsigned(7 downto 0);
    arg2 : unsigned(7 downto 0);
  end record; 

  subtype translated_address is unsigned(31 downto 0);
  subtype opcode_type is std_logic_vector(8 downto 0);

  subtype length_of_instruction is integer range 1 to 3;
  type ilens4 is array (0 to 3) of length_of_instruction;
  type bytes4 is array (0 to 3) of opcode_type;

  -- 16 byte buffer for fetching instructions from memory
  constant BYTE_BUFFER_WIDTH : integer := 16;
  type ilens is array (0 to BYTE_BUFFER_WIDTH) of integer;
  type prefetch_byte is record
    byte : opcode_type;
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
    
end package;

