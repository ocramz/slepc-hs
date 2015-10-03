{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Numerical.SLEPc.Raw.InlineC where
-- | foreign signatures, + everything that requires an inline-c pass

import Numerical.SLEPc.Raw.Internal

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

context slepcCtx

C.include "<slepceps.h>"
C.include "<slepcsvd.h>"
C.include "<petscmat.h>"


-- petscDecide = -1           -- don't ask

data PetscJudge = PetscJ { unPetscJ :: CInt}
{-# NOINLINE petscDecide #-}
petscDecide = PetscJ $ unsafePerformIO [C.exp|int{PETSC_DECIDE}|]
{-# NOINLINE petscDetermine #-}
petscDetermine = PetscJ $ unsafePerformIO [C.exp|int{PETSC_DETERMINE}|]



-- * Vec













-- * Mat


matCreate' comm = withPtr $ \p -> [C.exp| int{MatCreate($(int c), $(Mat *p))} |] 
  where c = unComm comm
matCreate1 = matCreate' 

matDestroy' m = [C.exp|int{MatDestroy($(Mat *m))}|]
matDestroy1 m = with m matDestroy' 

matSetup' m = [C.exp|int{MatSetUp($(Mat m))}|]


-- MatSetOption(A, MAT_NEW_NONZERO_ALLOCATION_ERR, PETSC_FALSE)
matNewNZallocError' a q =
  [C.exp|int{MatSetOption($(Mat a), MAT_NEW_NONZERO_ALLOCATION_ERR, $(PetscBool q))}|]

matNewNZallocErrorOff' a =
  [C.exp|int{MatSetOption($(Mat a), MAT_NEW_NONZERO_ALLOCATION_ERR, PETSC_FALSE)}|]

-- -- assembly:
-- --   type of assembly, either MAT_FLUSH_ASSEMBLY or MAT_FINAL_ASSEMBLY

-- PetscErrorCode  MatAssemblyBegin(Mat mat,MatAssemblyType type)
matAssemblyBegin' m = [C.exp|int{MatAssemblyBegin($(Mat m), MAT_FINAL_ASSEMBLY)}|]

-- PetscErrorCode  MatAssemblyEnd(Mat mat,MatAssemblyType type)
matAssemblyEnd' m = [C.exp|int{MatAssemblyEnd($(Mat m), MAT_FINAL_ASSEMBLY)}|]



-- PetscErrorCode  MatCreateSeqAIJ(MPI_Comm comm,PetscInt m,PetscInt n,PetscInt nz,const PetscInt nnz[],Mat *A)  -- Collective on MPI_Comm
-- Input Parameters :
-- comm	- MPI communicator, set to PETSC_COMM_SELF
-- m	- number of rows
-- n	- number of columns
-- nz	- number of nonzeros per row (same for all rows) (if nnz is given nz is ignored)
-- nnz	- array containing the number of nonzeros in the various rows (possibly different for each row) or NULL
-- Output Parameter :
-- A -the matrix 

matCreateSeqAIJ1 comm m' n' nnz' =
  withPtr (\mat ->
   withArray nnz $ \nnzp ->
            [C.exp|int{MatCreateSeqAIJ($(int c),
                                       $(PetscInt m),
                                       $(PetscInt n),
                                       0 ,
                                       $(PetscInt* nnzp),
                                       $(Mat *mat))}|]) 
  where c = unComm comm
        (m, n, nnz) = (toCInt m', toCInt n', map toCInt nnz')

matCreateSeqAIJconstNZperRow1 comm m' n' nz' =
  withPtr (\mat ->
            [C.exp|int{MatCreateSeqAIJ($(int c),
                                       $(PetscInt m),
                                       $(PetscInt n),
                                       $(PetscInt nz),
                                       NULL ,
                                       $(Mat *mat))}|]) 
  where c = unComm comm
        (m, n, nz) = (toCInt m', toCInt n', toCInt nz')

-- PetscErrorCode  MatGetOwnershipRange(Mat mat,PetscInt *m,PetscInt *n)
matGetOwnershipRange' mat = 
  withPtr ( \m ->
   withPtr $ \n ->
    [C.exp|int{MatGetOwnershipRange($(Mat mat),$(int* m),$(int* n))}|] ) >>= \(a, (b, c)) -> return ((a, b), c)


-- PetscErrorCode MatSetValue(Mat m,PetscInt row,PetscInt col,PetscScalar value,InsertMode mode)
matSetValueUnsafe' m row col val im =
  [C.exp|int{MatSetValue($(Mat m),$(int rowc),$(int colc),$(PetscScalar val),$(int imm))}|] where
    imm = fromIntegral $ insertModeToInt im
    rowc = toCInt row
    colc = toCInt col
    

-- PetscErrorCode  MatSetValues(Mat mat,PetscInt m,const PetscInt idxm[],PetscInt n,const PetscInt idxn[],const PetscScalar v[],InsertMode addv)
matSetValues0 mat nbx idxx_ nby idxy_ b_ im =
  [C.exp|int { MatSetValues($(Mat mat),
                      $(int nbxc),
                      $(int* idxx_),
                      $(int nbyc),
                      $(int* idxy_),
                      $(PetscScalar* b_), $(int imm))} |] where
    imm = fromIntegral $ insertModeToInt im
    nbxc = toCInt nbx
    nbyc = toCInt nby

matSetValuesUnsafe' ::
  Mat -> [Int] -> [Int] -> [PetscScalar_] -> InsertMode_ -> IO CInt
matSetValuesUnsafe' mat idxx_ idxy_ b im=
     withArray idxxc $ \idxxp ->
     withArray idxyc $ \idxyp ->
     withArray b $ \b_ ->
     matSetValues0 mat nbx idxxp nby idxyp b_ im 
  where
       nbx = length idxxc
       nby = length idxyc
       idxxc = map toCInt idxx_
       idxyc = map toCInt idxy_





matViewStdout' v = [C.exp|int{MatView($(Mat v), PETSC_VIEWER_STDOUT_SELF)}|]





















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

-- | sets subspace (array of Vec's) from which EPSSolve starts to iterate
-- PetscErrorCode EPSSetInitialSpace(EPS eps,PetscInt n,Vec *is)
epsSetInitialSpace' e subspace = withArray subspace $ \isp -> 
  [C.exp|int{EPSSetInitialSpace($(EPS e),$(int nc),$(Vec* isp))}|]
    where nc = toCInt n
          n = length subspace

-- EPSDestroy(EPS eps);
epsDestroy' e = with e $ \ep -> [C.exp|int{EPSDestroy($(EPS* ep))}|]

-- PetscErrorCode EPSView(EPS eps,PetscViewer viewer)
epsView' eps v = [C.exp|int{EPSView($(EPS eps),$(PetscViewer v))}|]










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
svdGetSingularTriplet' s i u v =
  withPtr $ \sig ->
  [C.exp|int{SVDGetSingularTriplet($(SVD s),$(int i),$(PetscReal* sig),$(Vec u),$(Vec v))}|]

-- SVDDestroy(SVD svd);
svdDestroy' s = with s $ \sp -> [C.exp|int{SVDDestroy($(SVD* sp))}|]













-- * SLEPc misc

-- SlepcInitialize(int *argc,char ***argv,char *file,char *help);
-- ierr = SlepcFinalize();

slepcInitialized' = withPtr ( \b ->
     [C.exp|int{ SlepcInitialized($(PetscBool * b)) } |] )   

  



slepcInit01 = [C.exp| int{ SlepcInitializeNoArguments()  }|]

slepcInitialize' args opts help = 
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
withSlepc' a o h = bracket_ (slepcInitialize' a o h) slepcFin1

withSlepc'' a o h f = slepcInitialize' a o h >> (f `finally` slepcFin1)








-- * MPI misc

mpiCommSize' comm = withPtr (\p -> [C.exp| int{ MPI_Comm_size($(int c), $(int *p))}|] )
  where
   c = unComm comm
-- mpiCommSize c =  unsafePerformIO $ mpiCommSize' c 

mpiCommRank' comm =
  withPtr
   (\p ->
     [C.exp| int{ MPI_Comm_rank($(int c), $(int *p))}|] ) 
  where
   c = unComm comm

-- mpiCommRank c = MkRank $ unsafePerformIO $ mpiCommRank' c 


{-# NOINLINE commWorld1 #-}
commWorld1 = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_WORLD }  |] 
commSelf1 = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_SELF }  |]
