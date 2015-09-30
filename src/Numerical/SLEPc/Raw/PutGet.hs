module Numerical.SLEPc.Raw.PutGet where

import Numerical.SLEPc.Raw.InlineC
import Numerical.SLEPc.Raw.Types
import Numerical.SLEPc.Raw.Utils
import Numerical.SLEPc.Raw.Exception
import Numerical.SLEPc.Internal

import Control.Monad
import Control.Concurrent
import Control.Exception




-- * SLEPc misc

-- withSlepc0 = bracketChk slepcInit01 slepcFin1
