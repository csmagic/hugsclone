-----------------------------------------------------------------------------
-- References for the IO monad:
--
-- This file contains (non-standard) hooks for an implementation of
-- mutable variables, or references, in Hugs 1.3.  These operations
-- are only available if the version of Hugs that you are using was
-- built with the IO_REFS flag set.
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module IORef where

data Ref a       -- builtin type of IO references

primitive newRef :: a -> IO (Ref a)
primitive getRef :: Ref a -> IO a
primitive setRef :: Ref a -> a -> IO ()
primitive eqRef  :: Ref a -> Ref a -> Bool

instance Eq (Ref a) where
    (==) = eqRef

-----------------------------------------------------------------------------
