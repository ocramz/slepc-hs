{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Numerical.SLEPc.Raw.PutGet
       -- (Comm, commWorld, commSelf,
       --  Mat, Vec, EPS, SVD)
       where

import Numerical.SLEPc.Raw.InlineC
import Numerical.SLEPc.Raw.Types
import Numerical.SLEPc.Raw.Utils
import Numerical.SLEPc.Raw.Exception
import Numerical.SLEPc.Raw.Internal

import Foreign.C.Types

import Control.Monad
import Control.Concurrent
import Control.Exception

import Control.Arrow ((***),(&&&))

import qualified Data.Graph as G
import qualified Data.Vector as V


-- * Vec




-- * Mat



withMat :: Comm -> (Mat -> IO a) -> IO a
withMat comm = bracketChk (matCreate1 comm) matDestroy1

matCreate :: Comm -> IO Mat
matCreate comm = chk1 (matCreate1 comm)

matDestroy :: Mat -> IO ()
matDestroy = chk0 . matDestroy1

matSetup = chk0 . matSetup'


matAssemblyBegin = chk0 . matAssemblyBegin'
matAssemblyEnd = chk0 . matAssemblyEnd'

matAssembly = matAssemblyBegin >> matAssemblyEnd

withMatAssembly m f = do
  matAssemblyBegin m
  f 
  matAssemblyEnd m



-- matNewNZallocError' a q
matNewNZallocError a q = chk0 (matNewNZallocError' a q)

matNewNZallocErrorOff = matNewNZallocErrorOff'

matDestroyPM (PetscMatrix _ m) = matDestroy m

data MatrixInfoBase =
  MatrixInfoBase {matComm :: Comm,
                  matRows :: !Int,
                  matCols :: !Int}

data MatrixInfo =
  MIConstNZPR MatrixInfoBase !Int
  | MIVarNZPR MatrixInfoBase ![Int]


-- | a datatype encapsulating matrix information and the typed pointer
data PetscMatrix = PetscMatrix !MatrixInfo Mat


petscMatrixBounds pm = petscMatrixInfoBBounds (petscMatrixInfoB pm) where
 petscMatrixInfoBBounds mi = (ibx, iby) where
  ibx = (0, matRows mi - 1) :: (Int, Int)
  iby = (0, matCols mi - 1) :: (Int, Int)

petscMatrixInfoB :: PetscMatrix -> MatrixInfoBase
petscMatrixInfoB (PetscMatrix (MIConstNZPR mi _) _) = mi
petscMatrixInfoB (PetscMatrix (MIVarNZPR mi _) _) = mi

petscMatrixMat :: PetscMatrix -> Mat
petscMatrixMat (PetscMatrix (MIConstNZPR _ _ ) m) = m
petscMatrixMat (PetscMatrix (MIVarNZPR _ _ ) m) = m





validDims0 :: Int -> Int -> Bool
validDims0 nr nc = nr > 0 && nc > 0 

validDims' :: MatrixInfoBase -> Bool
validDims' mi = validDims0 nr nc
      where (nr, nc) = (matRows &&& matCols) mi

validDims :: MatrixInfo -> Bool
validDims (MIConstNZPR mi nz) = validDims' mi && nz >= 0 && nz <= matCols mi
validDims (MIVarNZPR mi nnz) =
  validDims' mi && length nnz == matRows mi && all withinCols nnz where
    withinCols x = x >= 0 && x <= matCols mi

mkMatrixInfoBase comm nr nc
  | validDims0 nr nc = MatrixInfoBase comm nr nc
  | otherwise = error "mkMatrixInfoBase : matrix sizes must be > 0 " 

mkMatrixInfoConstNZPR :: Comm -> Int -> Int -> Int -> MatrixInfo
mkMatrixInfoConstNZPR comm nr nc = MIConstNZPR (mkMatrixInfoBase comm nr nc)

mkMatrixInfoVarNZPR :: Comm -> Int -> Int -> [Int] -> MatrixInfo
mkMatrixInfoVarNZPR comm nr nc = MIVarNZPR (mkMatrixInfoBase comm nr nc)

mkMatrix :: (Num a, Eq a) => MatrixInfo -> IO (Mat, a) -> IO PetscMatrix
mkMatrix mi matcreatef
  | validDims mi = do
      m <- chk1 matcreatef
      return $ PetscMatrix mi m
  | otherwise = error "mkMatrix : invalid sizes : no matrix allocated"

matCreateSeqAIJConstNZPR :: Comm -> Int -> Int -> Int -> IO PetscMatrix
matCreateSeqAIJConstNZPR comm nr nc nz =
  mkMatrix
    (mkMatrixInfoConstNZPR comm nr nc nz)
    (matCreateSeqAIJconstNZperRow1 comm nr nc nz)

matCreateSeqAIJVarNZPR :: Comm -> Int -> Int -> [Int] -> IO PetscMatrix
matCreateSeqAIJVarNZPR comm nr nc nnz =
  mkMatrix
    (mkMatrixInfoVarNZPR comm nr nc nnz)
    (matCreateSeqAIJ1 comm nr nc nnz)






-- -- | size- and memory- safe parallel CSR matrix brackets

-- | ", constant # of nonzeros / row

withMatCreateSeqAIJConstNZPR ::
  Comm -> Int -> Int -> Int ->
  (PetscMatrix -> IO a) ->
  IO a
withMatCreateSeqAIJConstNZPR comm nr nc nz =
  bracket (matCreateSeqAIJConstNZPR comm nr nc nz) matDestroyPM 

-- | ", variable # of nonzeros / row, specified in array nnz
withMatCreateSeqAIJVarNZPR ::
  Comm -> Int -> Int -> [Int] ->
  (PetscMatrix -> IO a) ->
  IO a
withMatCreateSeqAIJVarNZPR comm nr nc nnz =
  bracket (matCreateSeqAIJVarNZPR comm nr nc nnz) matDestroyPM




matSetValuesUnsafe ::
  Mat ->
  [Int] ->             -- x indices
  [Int] ->             -- y indices
  [PetscScalar_] ->    -- values 
  InsertMode_ ->       -- `InsertValues` or `AddValues`
  IO ()
matSetValuesUnsafe mat idxx idxy b im = chk0 $ matSetValuesUnsafe' mat idxx idxy b im





inBounds (imin, imax) i = i >= imin && i <= imax
inBounds_ ib = all (inBounds ib)




matSetValuesSafe ::
  PetscMatrix -> [Int] -> [Int] -> [PetscScalar_] -> InsertMode_ -> IO ()
matSetValuesSafe pm idxx idxy b im 
  | validIdxs && compatLengths = matSetValuesUnsafe m idxx idxy b im
  | otherwise = error "matSetValuesSafe : incompatible dimensions"
     where
      (ibx, iby) = petscMatrixBounds pm
      m = petscMatrixMat pm
      (lix, liy, lb) = (length idxx, length idxy, length b)
      compatLengths = (lix == liy) && (lix == lb)
      validIdxs = inBounds_ ibx idxx && inBounds_ iby idxy


matSetValuesSafeV ::
  PetscMatrix ->
  V.Vector Int ->
  V.Vector Int ->
  V.Vector PetscScalar_ ->
  InsertMode_ ->
  IO ()
matSetValuesSafeV pm idxx idxy b im 
  | validIdxs && compatLengths = matSetValuesUnsafe m idxxa idxya ba im
  | otherwise = error "matSetValuesSafe : incompatible dimensions"
     where
      (ibx, iby) = petscMatrixBounds pm
      m = petscMatrixMat pm
      (lix, liy, lb) = (V.length idxx, V.length idxy, V.length b)
      compatLengths = (lix == liy) && (lix == lb)
      validIdxs = inBoundsV_ ibx idxx && inBoundsV_ iby idxy
      idxxa = V.toList idxx
      idxya = V.toList idxy
      ba = V.toList b

inBoundsV_ ib = V.all (inBounds ib)

matViewStdout = chk0 . matViewStdout'








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
