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

withEpsSetupSolve :: Comm -> Mat -> Mat -> EpsProblemType_ -> (EPS -> IO a) -> IO a
withEpsSetupSolve comm a b ty postsolve = withEpsSetup comm a b ty $ \e -> do
  chk0 $ epsSolve' e
  postsolve e

epsIsHermitian, epsIsPositive :: EPS -> IO PetscBool_
epsIsHermitian = chk1 . epsIsHermitian'
epsIsPositive = chk1 . epsIsPositive'

epsSetDimensions :: EPS -> Int -> Int -> Int -> IO ()
epsSetDimensions e nev ncv mpd  = chk0 $ epsSetDimensions' e nev ncv mpd

epsSetInterval :: EPS -> PetscReal_ -> PetscReal_ -> IO ()
epsSetInterval e smin smax = chk0 $ epsSetInterval' e smin smax




epsView :: EPS -> PetscViewer -> IO ()
epsView e viewer = chk0 $ epsView' e viewer






-- * SVD

withSvd :: Comm -> (SVD -> IO a) -> IO a
withSvd comm = bracketChk (svdCreate' comm) svdDestroy'

withSvdSetup :: Comm -> Mat -> (SVD -> IO a) -> IO a
withSvdSetup comm oper act = withSvd comm $ \s -> do
  chk0 $ svdSetOperator' s oper
  act s

withSvdSetupSolve :: Comm -> Mat -> (SVD -> IO a) -> IO a
withSvdSetupSolve comm oper postsolve = withSvdSetup comm oper $ \s -> do
  chk0 $ svdSolve' s
  postsolve s
  








-- * SLEPc misc

slepcInit :: Argv -> OptsStr -> HelpStr -> IO ()
slepcInit a o h = chk0 $ slepcInitialize' a o h

slepcInit0, slepcFin :: IO ()
slepcInit0 = chk0 slepcInit01
slepcFin = chk0 slepcFin1


-- | FIXME: move into specialized monad
withSlepc0 :: IO a -> IO a
withSlepc0 = bracket_ slepcInit0 slepcFin

withSlepc :: Argv -> OptsStr -> HelpStr -> IO a -> IO a
withSlepc a o h = bracket_ (slepcInit a o h) slepcFin


commWorld, commSelf :: Comm
commWorld = commWorld1
commSelf = commSelf1
