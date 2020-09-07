-----------------------------------------------------------------------------
-- Standard Library: IO operations, beyond those included in the prelude
--
-- WARNING: The names and semantics of functions defined in this module
-- may change as the details of the IO standard are clarified.
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module IO where

-- Functions for inspecting IOErrors:

primitive isUserError       :: IOError -> Maybe String
primitive isIllegalError,
	  isAlreadyExists,
	  isAlreadyInUse,
	  isFullError,
	  isEOFError,
	  isPermissionError :: IOError -> Bool
primitive ioeGetHandle      :: IOError -> Maybe Handle
primitive ioeGetFileName    :: IOError -> Maybe FilePath

-- Various handle-oriented operations:

data Handle            -- builtin datatype of IO handles

data IOMode = ReadMode | WriteMode | AppendMode
	      deriving (Eq, Ord, Enum, Show, Read)

primitive stdin        :: Handle
primitive stdout       :: Handle
primitive stderr       :: Handle
primitive hGetContents :: Handle -> IO String
primitive openFile     :: FilePath -> IOMode -> IO Handle
primitive hClose       :: Handle -> IO ()
primitive hFlush       :: Handle -> IO ()
primitive hIsEOF       :: Handle -> IO Bool
primitive hPutChar     :: Handle -> Char -> IO ()
primitive hPutStr      :: Handle -> String -> IO ()
primitive hGetChar     :: Handle -> IO Char
primitive getCh        :: IO Char

isEOF                       :: IO Bool
isEOF                        = hIsEOF stdin

-----------------------------------------------------------------------------
