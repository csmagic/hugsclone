-----------------------------------------------------------------------------
-- Standard Library: System operations
--
-- Warning: the implementation of these functions in Hugs 1.3 is very weak.
-- The functions themselves are best suited to uses in compiled programs,
-- and not to use in an interpreter-based environment like Hugs.
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module System where

data ExitCode = ExitSuccess | ExitFailure Int
                deriving (Eq, Ord, Read, Show)

getArgs      :: IO [String]
getArgs       = return []       -- no args available

getProgName  :: IO String
getProgName   = return "Hugs"   -- provides a way for executing Haskell
                                -- programs to test if they are running
                                -- under Hugs ...

getEnv       :: String -> IO String
getEnv        = error "getEnv function is not implemented in Hugs"

system       :: String -> IO ExitCode
system        = error "system function is not implemented in Hugs"

exitWith     :: ExitCode -> IO a
exitWith      = error "exitWith function is not implemented in Hugs"

-----------------------------------------------------------------------------
