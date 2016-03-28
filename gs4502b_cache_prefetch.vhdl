-- This module loads instructions into the instruction-cache.
-- It is almost, but not quite, that simple.
--
-- Under normal operation, this unit fetches four bytes from the appropriate
-- memory space each cycle, and then creates the cache line corresponding to
-- the instruction beginning at the address of the first byte, and then submits
-- it for writing to the instruction-cache. This means that we can fill the
-- cache at least as fast as instructions can be dispatched, although as will
-- become apparent, this is only true on average.
--
-- However, there are a number of complications:
-- 1. If we hit an RTS or RTI instruction, there isn't any point parsing
-- further. Well, at least unless we know where the RTS is going to go.  For
-- this reason, we should keep track of where JSR instructions are when
-- following them to their sub-routines.  That way, we can trace-back, and
-- continue populating the instruction cache, to avoid misses being otherwise
-- incurred.
--
-- 2. Because we implement instruction merging, which means that if three
-- instructions are merged, the cache line will be written only on the 3rd cycle.
-- Thus the CPU might sit idle for a while, but when it then gets the merged
-- instructions, they will end up being executed in no more time than if we sent
-- them out one after the other.  The positive side is that if the merged
-- instructions are executed more than once, then we start saving cycles. This
-- is especially promising for tight loops, where we can most likely merge the
-- post-index with the store instruction. If we get really clever, we might
-- even be able to merge the branch instruction, but that will require
-- something like a "not-quite-zero" instruction flag, and is left as a future
-- exercise.
-- 
-- 3. Every memory write could be an instance of self-modifying code. There
-- just isn't any way to know. Thus, for EVERY memory write, we have to check
-- three successive cache lines (because any given byte could be the opcode,
-- first argument or second argument byte of an instruction), and if necessary,
-- patch the already-cached instructions so that they reflect the result of the
-- memory write. This means stalling any existing pre-fetch exercise, reading
-- the lines in question in parallel, and then queing any corrects for
-- re-processing and write-back. It might be possible to optimise this by only
-- patching instructions when we can get away with it, e.g., if the arguments
-- are of a load or store instruction.  It is if the arguments of a branch, or
-- if the opcode of an instruction changes, that we have a bigger problem, that
-- requires the whole instruction to be re-fetched. This has to be done
-- promptly, because the instruction pipeline will only wait for a certain
-- number of cycles when it detects a self-modification hazard.  After that
-- timeout, the instruction cache MUST be presenting the correct version.  The
-- yuckiest logic is in the pipeline.
--
-- 4. The CPU could indicate a cache miss at anytime, in which case we need to
-- finish what we are doing, and quickly start fetching instructions from the
-- new location.  This will probably be handled just as though it were a normal
-- cache miss.
--
-- Further complications for the pre-fetcher come from the fact that when we
-- know the address for the next instruciton, we don't actually know how many
-- bytes long it is.  That information probably won't be available for two more
-- cycles, while we request the bytes for this instruction, and then decode it.
-- From a simplistic perspective, this would mean that we can only load the
-- cache with a new instruction every 3 cycles or so.  Loading it faster would
-- require speculating about the length of the next instruction.  Such
-- speculation would be correct, on average, only 1 in 3 instructions, which
-- means that 1/3 instructions could be pre-fetched in 1 cycle, while the other
-- 2/3 would take 4 cycles.  This would give an overall average of about 3
-- cycles per instruction.  That said, a quick analysis of the C64 kernal shows
-- that over 52% of all instructions are 2 bytes long, so there is considerable
-- potential for an intelligent instruction length speculator.  However, there
-- is considerable variablility, if we look at the figures for each 256
-- instructions in the C64 kernal:
--
-- onebyte twobyte threebyte total %onebyte %twobyte %threebyte
-- 35      141     80        256   13.6719  55.0781 31.25
-- 40      144     72        256   15.625   56.25   28.125
-- 58      163     35        256   22.6562  63.6719 13.6719
-- 38      171     47        256   14.8438  66.7969 18.3594
-- 50      154     52        256   19.5312  60.1562 20.3125
-- 48      116     92        256   18.75    45.3125 35.9375
-- 42      142     72        256   16.4062  55.4688 28.125
-- 55      139     62        256   21.4844  54.2969 24.2188
-- 27      154     75        256   10.5469  60.1562 29.2969
-- 52      132     72        256   20.3125  51.5625 28.125
-- 49      91      113       256   19.1406  35.5469 44.1406
-- 39      39      112       190   20.5263  20.5263 58.9474
--
-- That said, two-byte instructions still largely dominate.  The main exception
-- is at the end of the ROM which is full of 3-byte instructions for the kernel
-- jump table. The prevalence of 2-byte instructions makes some sense, since
-- all relative branches (which alone constitute more than 14% of the kernal) and
-- immediate mode and zero-page instructions are two bytes long.
--
-- However, even with a speculative fetcher, we still have the problem that we
-- would need a way to flush out mis-predicted instruction boundaries.  Then
-- there are jumps and branches and things, which constitute about every third
-- instruction on average (again, using the C64 kernal as a code sample).
--
-- Given that we really would like to get the highest benchmark scores
-- possible, which to perform well in SynthMark64 means that we have to be able
-- to execute an instruction most cycles if we want to hit performance figures
-- of several hundred times stock speed, since SynthMark64 only runs a given
-- block of code twice, so it will only be loaded in the cache once each
-- time... unless we use memory writes to trigger cache loads (which we have to
-- do, at least in terms of invalidating cache lines affected by writes), but
-- actively loading the cache with the 3 three instructions that any given byte
-- could be part of sounds like a great way to optimise SynthMark64 results,
-- while pessimising real-world performance.  Also, we are not interested in
-- cheating: we want this processor to be as smokingly fast as it can be,
-- regardless of the workload.
--
-- But the question is, can we implement something that would almost always be
-- fetching a useful instruction?  Can we use the high frequency of branching
-- instructions to some advantage?  While it won't necessarily help with the
-- SynthMark64 tests which purposely consist of various unrolled loops so as to
-- defeat caches, it would likely help in the real world if we speculatively
-- cache instructions on both sides of a branch.  That is, we can keep note of
-- alternative PC values that would arise through the parsing process, and
-- whenever the fetch logic has nothing better to do, that will provably be the
-- correct address of an instruction in the probable instruction stream, then
-- it can pull one of these off the list.  Once there are three or four such
-- targets being explored in parallel, the cache pre-fetch logic can simply be
-- cycling between them, each time the valid address is known for the next
-- instruction, in effect performing a breadth-first traversal of the program.
--
-- This sounds like a good approach, as it can, in principle, keep loading the
-- cache with a new instruction every single cycle. In fact, the main problem
-- it is likely to face is that it will eventually load every reachable
-- instruction into the cache!  While cache misses will naturally help to
-- refocus its attention, such misses are likely to occur fairly often,
-- precisely because it would end up ejecting a lot of instructions.  So we
-- would need some way to limit the extent of this breadth-first search.  We
-- could put instruction count limits, depth limit for side-branches or
-- some homehow otherwise limit the number of side-branches that will be
-- explored.
--
-- Another approach would be to pre-fetch sufficient bytes in a shift-buffer,
-- so that we can emit one valid instruction per cycle, using the look-ahead to
-- resolve the address boundaries.  This should, in fact, be much simpler, as
-- it can emit one valid instruction per cycle, and solve all of these above
-- problems, both for real work-loads and benchmarks.
--
-- First, we need to have enough bytes to keep the look-ahead primed.  This
-- means we need to always have three bytes to look at.
-- Second, we can always read the next four bytes while looking at what we
-- have, and because we will have done this in the last cycle as well, we can
-- shift in the extra bytes that we need.
--
-- So the logic here sounds like it should be really quite simple.  

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;
use work.icachebits.all;

entity gs4502b_cache_prefetch is
  port (
    cpuclock : in std_logic;

    cpu_personality : in cpu_personality;
    
    -- Was there a cache miss?
    cache_miss : in boolean := false;
    cache_miss_address : in translated_address;
    cache_miss_pch : in unsigned(15 downto 8);
    
    -- XXX Interface to instruction cache
    -- Instruction cache must be four parallel BRAM structures, so that we can
    -- read all three relevant cache lines in a single cycle after a memory
    -- write occurs.
    icache_write_enable : out std_logic := '0';
    icache_address : out std_logic_vector(9 downto 0);
    icache_wdata : out std_logic_vector(107 downto 0);
    icache_rdata : in std_logic_vector(107 downto 0)
    
    );
end gs4502b_cache_prefetch;

architecture behavioural of gs4502b_cache_prefetch is
  signal instruction_address : translated_address := (others => '0');
  signal instruction_pch : unsigned(15 downto 8) := x"81";
  signal last_cache_miss_pch : unsigned(15 downto 8) := (others => '0');
  signal last_cache_miss_address : translated_address := (others => '0');
  constant CACHE_MISS_COUNTER_MAX : integer := 15;
  signal cache_miss_counter : integer range 0 to CACHE_MISS_COUNTER_MAX := 0;
  signal repeated_cache_miss_address : boolean := false;
  signal suppress_cache_miss : boolean := false;
begin
  process (cpuclock) is
    variable icache_line : std_logic_vector(107 downto 0);
  begin
    if rising_edge(cpuclock) then
      
      -- Check if there is a cache miss. If yes, then set next instruction
      -- address to the cache miss address, and continue fetching from there.

      -- Well, it isn't actually that simple. The cache miss signal can be
      -- presented for several cycles (basically from when the miss is noticed,
      -- until the decode stage spots that we have put the right instruction in
      -- the cache again), in which case we would be clamped to the cache miss
      -- address until that releases, wasting cycles that could be spent fetching
      -- the instructions that follow the cache miss, as otherwise the process will
      -- repeat, and the cache will never be ready ahead of time on the first run
      -- through a series of instructions, and performance will be horrible.
      -- Thus
      suppress_cache_miss <= false;
      if cache_miss then
        if (last_cache_miss_address = cache_miss_address)
          and (last_cache_miss_pch = cache_miss_pch) then
          repeated_cache_miss_address <= true;
        else
          repeated_cache_miss_address <= false;
        end if;
        if repeated_cache_miss_address then
          suppress_cache_miss <= true;
        end if;
      end if;
      
      if cache_miss and (not suppress_cache_miss) then
        report "$" & to_hstring(cache_miss_address) & " PREFETCH : " &
          "Cache miss detected for PC=$" &
          to_hstring(cache_miss_pch) & to_hstring(cache_miss_address(7 downto 0));
        -- Proceed from indicated cache_miss address

        instruction_address <= cache_miss_address;
        instruction_pch <= cache_miss_pch;
        last_cache_miss_address <= cache_miss_address;
        last_cache_miss_pch <= cache_miss_pch;
        cache_miss_counter <= CACHE_MISS_COUNTER_MAX;
      else
        -- Otherwise, keep fetching from where we were.
        -- XXX Dummy incremental advance of instruction address
        instruction_address <= instruction_address + 1;
        if (instruction_address(7 downto 0) = x"FF") then
          instruction_pch <= instruction_pch + 1;
        end if;
        -- ... and reduce cache miss supression counter
        if cache_miss_counter > 0 then
          cache_miss_counter <= cache_miss_counter - 1;
        end if;
      end if;

      -- XXX Dummy code to prepare simple dummy icache lines to feed to
      -- processor during early testing. This must be replaced with the
      -- multi-stage cache fetch pipeline that does the required memory
      -- requests, and actually prepares the instruction entry for putting into
      -- the cache.

      report "$" & to_hstring(instruction_address) & " CACHE-FETCH";
      
      icache_line := (others => '0');
      icache_line(ICACHE_INSTRUCTION_ADDRESS_MAX downto ICACHE_INSTRUCTION_ADDRESS_START)
        := std_logic_vector(instruction_address(31 downto 10));
      icache_line(ICACHE_INSTRUCTION_BYTES_MAX downto ICACHE_INSTRUCTION_BYTES_START)
        := x"EAEAEA";
      icache_line((ICACHE_PC_EXPECTED_START+7) downto ICACHE_PC_EXPECTED_START)
        := std_logic_vector(instruction_address(7 downto 0));
      icache_line(ICACHE_PC_EXPECTED_MAX downto (ICACHE_PC_EXPECTED_MAX-7))
        := std_logic_vector(instruction_pch);
      icache_line((ICACHE_PC_MISPREDICT_START+7) downto ICACHE_PC_MISPREDICT_START)
        := std_logic_vector(instruction_address(7 downto 0));
      icache_line(ICACHE_PC_MISPREDICT_MAX downto (ICACHE_PC_MISPREDICT_MAX-7))
        := std_logic_vector(instruction_pch);
      icache_line(ICACHE_PCH_MAX downto ICACHE_PCH_START)
        := std_logic_vector(instruction_pch);

      icache_line(ICACHE_BRANCH_PREDICT) := '0';
      
      icache_line(ICACHE_CPU_PERSONALITY_MAX downto ICACHE_CPU_PERSONALITY_START)
        := to_std_logic_vector(cpu_personality);

      icache_wdata  <= icache_line;
      icache_write_enable <= '1';
      icache_address <= std_logic_vector(instruction_address(9 downto 0));
      
    end if;
  end process;
  
end behavioural;
