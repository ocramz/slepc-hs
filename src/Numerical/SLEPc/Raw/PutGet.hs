module Numerical.SLEPc.Raw.PutGet where

import Numerical.SLEPc.Raw.InlineC
import Numerical.SLEPc.Raw.Types
import Numerical.SLEPc.Raw.Utils
import Numerical.SLEPc.Raw.Exception
import Numerical.SLEPc.Raw.Internal

import Control.Monad
import Control.Concurrent
import Control.Exception


-- * Vec




-- * Mat




-- * EPS





-- * SLEPc misc

withSlepc0 = bracket_ slepcInit01 slepcFin1



commWorld = commWorld1
commSelf = commSelf1
