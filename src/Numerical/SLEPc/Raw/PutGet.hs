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

withEps :: Comm -> (EPS -> IO a) -> IO a
withEps comm = bracketChk (epsCreate' comm) epsDestroy'

withEpsSetup :: Comm -> Mat -> Mat -> EpsProblemType_ -> (EPS -> IO a) -> IO a
withEpsSetup comm matA matB ty post = withEps comm $ \e -> do
  chk0 $ epsSetOperators' e matA matB
  chk0 $ epsSetProblemType' e ty
  chk0 $ epsSetup' e
  post e

withEpsSetupSolve comm a b ty postsolve = withEpsSetup comm a b ty $ \e -> do
  chk0 $ epsSolve' e
  postsolve e

epsIsHermitian = chk1 . epsIsHermitian'

epsIsPositive = chk1 . epsIsPositive'

epsSetDimensions e nev ncv mpd  = chk0 $ epsSetDimensions' e nev ncv mpd

epsSetInterval e smin smax = chk0 $ epsSetInterval' e smin smax




epsView e viewer = chk0 $ epsView' e viewer









-- * SLEPc misc

slepcInit0 = chk0 slepcInit01
slepcFin = chk0 slepcFin1

withSlepc0 = bracket_ slepcInit0 slepcFin



commWorld = commWorld1
commSelf = commSelf1
