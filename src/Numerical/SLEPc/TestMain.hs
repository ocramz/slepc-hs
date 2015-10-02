module Numerical.SLEPc.TestMain where


import Numerical.SLEPc.Raw.PutGet
import Numerical.SLEPc.Raw.Types


t1' = withMatCreateSeqAIJVarNZPR cw 3 3 [1,1,1] $ \pm -> do
  let m = petscMatrixMat pm
  matSetValuesSafe pm [0,1,2] [0,1,2] (replicate 3 pi) InsertValues
  matViewStdout m
    where
      cw = commWorld

t1 = withSlepc0 t1'
