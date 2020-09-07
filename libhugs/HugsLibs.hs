-----------------------------------------------------------------------------
-- Dummy module to import all of the Hugs libraries; programmers should
-- normally be more selective than this when it comes to specifying the
-- modules that a particular program depends on.
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module HugsLibs where

import StdLibs
import ST
import STArray
import IORef
import Trace
import Number
import ParseLib
import Interact
import AnsiScreen
import AnsiInteract

-----------------------------------------------------------------------------
