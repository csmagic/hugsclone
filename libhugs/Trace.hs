-----------------------------------------------------------------------------
-- Trace primitive: import this library as a simple way to access the
-- impure trace primitive.  This is sometimes useful for debugging,
-- although understanding the output that it produces can sometimes be
-- a major challenge unless you are familiar with the intimate details
-- of how programs are executed.
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module Trace where

primitive trace :: String -> a -> a

-----------------------------------------------------------------------------
