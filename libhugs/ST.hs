-----------------------------------------------------------------------------
-- Lazy state threads:
--
-- This file contains (non-standard) hooks for an implementation of
-- the lazy state thread monad, ST, as described in the PLDI '94
-- paper by John Launchbury and Simon Peyton Jones.
--
-- These operations are only available if the version of Hugs that you
-- are using was built with the LAZY_ST flag set.
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module ST where

data MutVar s a     -- implemented as an internal primitive

primitive returnST     "STReturn"   :: a -> ST s a
primitive thenST       "STBind"     :: ST s a -> (a -> ST s b) -> ST s b
primitive newVar       "STNew"      :: a -> ST s (MutVar s a)
primitive readVar      "STDeref"    :: MutVar s a -> ST s a
primitive writeVar     "STAssign"   :: MutVar s a -> a -> ST s ()
primitive mutvarEq     "STMutVarEq" :: MutVar s a -> MutVar s a -> Bool
primitive interleaveST "STInter"    :: ST s a -> ST s a

instance Eq (MutVar s a) where (==) = mutvarEq

instance Monad (ST s) where
    return = returnST
    (>>=)  = thenST

-----------------------------------------------------------------------------
