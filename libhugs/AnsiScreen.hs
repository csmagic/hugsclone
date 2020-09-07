-----------------------------------------------------------------------------
-- Library of escape sequences for ANSI compatible screen I/O:
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module AnsiScreen where

-- Basic screen control codes:

-- Choose whichever of the following lines is suitable for your system:
cls         = "\ESC[2J"     -- for PC with ANSI.SYS
cls         = "\^L"         -- for Sun window

goto x y    = '\ESC':'[':(show y ++(';':show x ++ "H"))
at (x,y) s  = goto x y ++ s
home        = goto 1 1
highlight s = "\ESC[7m"++s++"\ESC[0m"

-----------------------------------------------------------------------------
