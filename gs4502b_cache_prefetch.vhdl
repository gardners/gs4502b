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

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.icachetypes.all;

entity gs4502b_cache_prefetch is
  port (
    cpuclock : in std_logic;

    -- Was there a cache miss?
    cache_miss : in boolean := false;
    cache_miss_address : in translated_address;
    cache_miss_pch : in unsigned(15 downto 8)
    
    -- XXX Interface to instruction cache
    -- Instruction cache must be four parallel BRAM structures, so that we can
    -- read all three relevant cache lines in a single cycle after a memory
    -- write occurs.
    
    );
end gs4502b_cache_prefetch;

architecture behavioural of gs4502b_cache_prefetch is
begin
  process (cpuclock) is
  begin
    if rising_edge(cpuclock) then
      if cache_miss then
        report "$" & to_hstring(cache_miss_address) & " PREFETCH : " &
          "Cache miss detected for PC=$" &
          to_hstring(cache_miss_pch) & to_hstring(cache_miss_address(7 downto 0));
      end if;
    end if;    
  end process;
  
end behavioural;
