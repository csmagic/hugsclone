-----------------------------------------------------------------------------
-- Mutable arrays for lazy state threads:
--
-- This file contains (non-standard) hooks for an implementation of
-- mutable arrays for the lazy state thread monad, ST, in Hugs 1.3.
-- These primitives are compatible with the ones described in the
-- PLDI '94 paper by John Launchbury and Simon Peyton Jones.
--
-- These operations are only available if the version of Hugs that you
-- are using was built with the LAZY_ST and HASKELL_ARRAYS flags set.
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module STArray where

import ST
import Array

data MutArr s a b -- implemented as primitive

primitive primSTNewArr   "STNewArr"
                  :: (a -> Int) -> (a,a) -> b -> ST s (MutArr s a b)
primitive primSTReadArr  "STReadArr"
                  :: ((a,a) -> a -> Int) -> MutArr s a b -> a -> ST s b
primitive primSTWriteArr "STWriteArr"
                  :: ((a,a) -> a -> Int) -> MutArr s a b -> a -> b -> ST s ()
primitive primSTFreeze   "STFreeze"
                  :: MutArr s a b -> ST s (Array a b)

newArr       :: Ix a => (a,a) -> b -> ST s (MutArr s a b)
newArr bounds = primSTNewArr (index bounds) bounds

readArr      :: Ix a => MutArr s a b -> a -> ST s b
readArr       = primSTReadArr index

writeArr     :: Ix a => MutArr s a b -> a -> b -> ST s ()
writeArr      = primSTWriteArr index

freezeArr    :: Ix a => MutArr s a b -> ST s (Array a b)
freezeArr     = primSTFreeze

-----------------------------------------------------------------------------
