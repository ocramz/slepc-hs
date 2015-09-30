{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Numerical.SLEPc.Raw.InlineC where
-- | foreign signatures, + everything that requires an inline-c pass

import Numerical.SLEPc.Internal

import Numerical.SLEPc.Raw.Types
import Numerical.SLEPc.Raw.Utils

import Language.C.Inline as C
import Language.C.Inline.Context
import Control.Exception
-- import Control.Concurrent
import Foreign
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Control.Monad
-- import Control.Arrow ((***), (&&&))
-- import Control.Applicative
import Foreign.C.Types
import Foreign.C.String
import qualified Foreign.ForeignPtr.Safe         as FPS

import System.IO.Unsafe (unsafePerformIO)

context petscCtx

C.include "<slepceps.h>"
C.include "<slepcsvd.h>"


-- petscDecide = -1           -- don't ask








-- * EPS


-- EPSCreate(MPI_Comm comm,EPS *eps);
epsCreate' comm = withPtr $ \e -> [C.exp|int{EPSCreate($(int c), $(EPS* e))}|] where
  c = unComm comm

-- EPSSetOperators(EPS eps,Mat A,Mat B);
epsSetOperators' e matA matB = [C.exp|int{EPSSetOperators($(EPS e),$(Mat matA),$(Mat matB))}|]
  
-- EPSSetProblemType(EPS eps,EPSProblemType type);
epsSetProblemType' e ty = [C.exp|int{EPSSetProblemType($(EPS e),$(int tyi))}|] where
  tyi = toCInt $ epsProblemTypeToInt ty

-- EPSSetUp(EPS eps)
epsSetup' e = [C.exp|int{EPSSetUp($(EPS e))}|]

-- EPSSetFromOptions(EPS eps);
  
-- EPSSolve(EPS eps);
epsSolve' e = [C.exp|int{EPSSolve($(EPS e))}|]

-- EPSGetConverged(EPS eps, int *nconv);
epsGetConverged' e = withPtr $ \nconv -> [C.exp|int{EPSGetConverged($(EPS e),$(int* nconv))}|]

-- EPSGetEigenpair(EPS eps,int i,PetscScalar *kr,PetscScalar *ki,Vec xr,Vec xi);
epsGetEigenpair' e i kr ki xr xi =
  [C.exp|int{EPSGetEigenpair($(EPS e),$(int i),$(PetscScalar* kr),$(PetscScalar* ki),$(Vec xr),$(Vec xi))}|]

-- | is the operator Hermitian?
epsIsHermitian' e = withPtr $ \ish -> [C.exp|int{EPSIsHermitian($(EPS e),$(PetscBool* ish))}|]

-- | is the operator positive definite?
epsIsPositive' e = withPtr $ \isp -> [C.exp|int{EPSIsPositive($(EPS e),$(PetscBool* isp))}|]


-- | set number of eigenvals to compute and subspace dimension:
-- nev : # eigenvalues
-- ncv : subspace dim
-- mpd : max. projected dimension
epsSetDimensions' e nev ncv mpd = [C.exp|int{EPSSetDimensions($(EPS e),$(int nevc),$(int ncvc),$(int mpdc))}|] where
  (nevc, ncvc, mpdc) = (toCInt nev, toCInt ncv, toCInt mpd)

epsSetInterval' e smin smax = [C.exp|int{EPSSetInterval($(EPS e),$(PetscReal smin),$(PetscReal smax))}|]


-- EPSDestroy(EPS eps);
epsDestroy' e = with e $ \ep -> [C.exp|int{EPSDestroy($(EPS* ep))}|]

-- PetscErrorCode EPSView(EPS eps,PetscViewer viewer)
epsView1 eps v = [C.exp|int{EPSView($(EPS eps),$(PetscViewer v))}|]










-- * ST -- spectral transformations

stCreate' comm = withPtr $ \s -> [C.exp|int{STCreate($(int c),$(ST* s))}|]
  where c = unComm comm

stSetType' st t =
  withCString ts $ \tp -> [C.exp|int{STSetType($(ST st),$(char* tp))}|]
    where ts = stTypeToString t

stDestroy' st = with st $ \stp -> [C.exp|int{STDestroy($(ST* stp))}|]










-- * SVD

-- | in real symmetric (or complex Hermitian) matrices, singular values coincide with eigenvalues, but in general this is not the case. The SVD is defined for any matrix, even rectangular ones. Singular values are always non-negative real values. 


-- SVDCreate(MPI_Comm comm,SVD *svd);
svdCreate' comm = withPtr $ \s -> [C.exp|int{SVDCreate($(int c),$(SVD* s))}|] where
  c = unComm comm
  
-- SVDSetOperator(SVD svd,Mat A);
svdSetOperator' s matA = [C.exp|int{SVDSetOperator($(SVD s),$(Mat matA))}|]
  
-- SVDSetFromOptions(SVD svd);

-- SVDSolve(SVD svd);
svdSolve' s = [C.exp|int{SVDSolve($(SVD s))}|]

-- SVDGetConverged(SVD svd, int *nconv);
svdGetConverged' s = withPtr $ \n -> [C.exp|int{SVDGetConverged($(SVD s),$(int* n))}|]

-- SVDGetSingularTriplet(SVD svd,int i,PetscReal *sigma,Vec u,Vec v);
svdGetSingularTriplet' s i =
  withPtr $ \sig ->
   withPtr $ \u ->
    withPtr $ \v -> 
  [C.exp|int{SVDGetSingularTriplet($(SVD s),$(int i),$(PetscReal* sig),$(Vec* u),$(Vec* v))}|]

-- SVDDestroy(SVD svd);
svdDestroy' s = with s $ \sp -> [C.exp|int{SVDDestroy($(SVD* sp))}|]







-- * SLEPc misc

-- SlepcInitialize(int *argc,char ***argv,char *file,char *help);
-- ierr = SlepcFinalize();

slepcInitialized' = withPtr ( \b ->
     [C.exp|int{ SlepcInitialized($(PetscBool * b)) } |] )   

  



slepcInit01 = [C.exp| int{ SlepcInitializeNoArguments()  }|]

slepcInitialize1 args opts help = 
 let acc = fromIntegral $ length args in 
  with acc $ \ac ->
   withCStringArray args $ \aa ->
   with aa $ \a ->
    withCString opts $ \o ->
    withCString help $ \h ->
    [C.exp|int{SlepcInitialize($(int *ac), $(char*** a), $(char* o), $(char *h))}|] 

type Argv = [String]
type OptsStr = String
type HelpStr = String

slepcFin1 = [C.block| int{ SlepcFinalize(); }|] 

withSlepc01 f = do    -- returns IO ()
  slepcInit01
  f
  slepcFin1

withPetsc0'' f =
  slepcInit01 >> (f `finally` slepcFin1)
  
withSlepc01' = bracket_ slepcInit01 slepcFin1 

withSlepc' :: Argv -> OptsStr -> HelpStr -> IO a -> IO a
withSlepc' a o h = bracket_ (slepcInitialize1 a o h) slepcFin1

withSlepc'' a o h f = slepcInitialize1 a o h >> (f `finally` slepcFin1)
