module Numerical.SLEPc.TestMain where


import Numerical.SLEPc.Raw.PutGet
import Numerical.SLEPc.Raw.Types


t1' = withMatCreateSeqAIJVarNZPR cw 3 3 [1,1,1] $ \pm -> do
  let m = petscMatrixMat pm
  matNewNZallocErrorOff m
  matSetup m
  matSetValuesSafe pm [0,1,2] [0,1,2] (replicate 3 pi) InsertValues
  matAssembly m
  matViewStdout m
    where
      cw = commWorld

t1 = withSlepc0 t1'


t2' = withMatCreateSeqAIJConstNZPR cw 3 3 3 $ \pm -> do
  let m = petscMatrixMat pm
  matNewNZallocErrorOff m
  matSetup m
  -- matSetValueSafe pm 1 1 pi InsertValues
  -- matSetValueArraySafe pm [0,1,2] [0,1,2] (replicate 3 pi) InsertValues
  matSetValueArraySafe pm [0,0,0,1,1,1,2,2,2] [0,1,2,0,1,2,0,1,2] (replicate 9 pi) InsertValues
  matAssembly m
  matViewStdout m
    where
      cw = commWorld

t2 = withSlepc0 t2'


t3' = withMatCreateSeqAIJConstNZPR cw 3 3 3 $ \pm -> do
  let m = petscMatrixMat pm -- last time copying all this stuff, will abstract out
  matNewNZallocErrorOff m -- <- need to figure this one out
  matSetup m -- <- this should be a given
  matSetValueArraySafe pm [0,0,0,1,1,1,2,2,2] [0,1,2,0,1,2,0,1,2] (replicate 9 pi) InsertValues
  matAssembly m
  withEpsSetupSolve cw m m EpsHep $ \eps ->
    epsIsHermitian eps
     where
       cw = commWorld

t3 = withSlepc0 t3'
