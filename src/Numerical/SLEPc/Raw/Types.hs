module Numerical.SLEPc.Raw.Types where

import Foreign
import Foreign.C.Types

-- * Vec


-- * Mat

data MatType_ = MatSame | MatMaij | MatSeqMaij | MatMPIMaij | MatIs | MatAij
              | MatSeqAij | MatSeqAijPThread | MatAijPthread | MatMPIAij -- etc.
                                                               deriving (Eq, Show)
matTypeToStr MatSame = "same"
matTypeToStr MatIs = "is"
matTypeToStr MatAij = "aij"
matTypeToStr MatMPIAij = "mpiaij"
matTypeToStr _ = "mpiaij" -- default


data MatCompositeType_ = MatCompositeAdditive | MatCompositeMultiplicative deriving (Eq, Show, Enum)
matCompositeTypeToInt x = fromEnum (x :: MatCompositeType_ )






-- * viewers


data PetscViewerType_ = ViewerSock | ViewerAscii | ViewerBinary | ViewerString | ViewerDraw | ViewerVu | ViewerMathematica | ViewerNetcdf | ViewerHdf5 | ViewerVtk | ViewerMatlab | ViewerSaws deriving (Eq, Enum, Show)

viewerTypeToInt x = fromEnum (x :: PetscViewerType_)

viewerTypeToStr ViewerSock = "socket"
viewerTypeToStr ViewerAscii = "ascii"
viewerTypeToStr ViewerHdf5 = "hdf5"
viewerTypeToStr ViewerNetcdf = "netcdf"






-- * EPS

data EpsType_ =
  EpsPower | EpsSubspace | EpsArnoldi | EpsLanczos | EpsKrylovSchur | EpsGd | EpsJd
  | EpsRqcg | EpsLobpcg | EpsCiss | EpsLapack | EpsArpack | EpsBlzpack | EpsTrlan
  | EpsBlopex | EpsPrimme | EpsFeast deriving (Eq, Show, Enum)

epsTypeToString t =
  case t of EpsPower -> "power"
            EpsSubspace -> "subspace"
            EpsArnoldi -> "arnoldi"
            EpsLanczos -> "lanczos"
            EpsKrylovSchur -> "krylovschur"
            EpsGd -> "gd"
            _ -> epsTypeToString EpsKrylovSchur -- default

-- typedef const char* EPSType;
-- #define EPSPOWER       "power"
-- #define EPSSUBSPACE    "subspace"
-- #define EPSARNOLDI     "arnoldi"
-- #define EPSLANCZOS     "lanczos"
-- #define EPSKRYLOVSCHUR "krylovschur"
-- #define EPSGD          "gd"
-- #define EPSJD          "jd"
-- #define EPSRQCG        "rqcg"
-- #define EPSLOBPCG      "lobpcg"
-- #define EPSCISS        "ciss"
-- #define EPSLAPACK      "lapack"
-- #define EPSARPACK      "arpack"
-- #define EPSBLZPACK     "blzpack"
-- #define EPSTRLAN       "trlan"
-- #define EPSBLOPEX      "blopex"
-- #define EPSPRIMME      "primme"
-- #define EPSFEAST       "feast"



-- typedef enum { EPS_HEP=1, EPS_GHEP, EPS_NHEP, EPS_GNHEP, EPS_PGNHEP, EPS_GHIEP } EPSProblemType;

data EpsProblemType_ = EpsHep | EpsGHep | EpsNHep | EpsGNHep | EpsPGNHep | EpsGHIep
                     deriving (Eq, Show, Enum)

epsProblemTypeToInt EpsHep = 1
epsProblemTypeToInt x = fromEnum (x :: EpsProblemType_ )


-- typedef enum { EPS_LARGEST_MAGNITUDE=1, EPS_SMALLEST_MAGNITUDE, EPS_LARGEST_REAL, EPS_SMALLEST_REAL, EPS_LARGEST_IMAGINARY, EPS_SMALLEST_IMAGINARY, EPS_TARGET_MAGNITUDE, EPS_TARGET_REAL, EPS_TARGET_IMAGINARY, EPS_ALL, EPS_WHICH_USER } EPSWhich;

data EpsWhich_ =
  LargestMag | SmallestMag | LargestReal | SmallestReal | LargestImag | SmallestImag
  | TargetMag | TargetReal | TargetImag | EpsWhichAll | EpsWhichUser deriving (Eq, Show, Enum)

epsWhichToInt LargestMag = 1
epsWhichToInt x = fromEnum (x :: EpsWhich_)






-- * SVD

data SvdType_ =
  SvdCross | SvdCyclic | SvdLanczos | SvdTRLanczos | SvdLapack
   deriving (Eq, Show, Ord)







-- * ST -- spectral transformations
data StType_ =
  StShell | StShift | StSInvert | StCayley | StPrecond deriving (Eq, Ord, Show)

stTypeToString t =
  case t of StShell -> "shell"
            StShift -> "shift"
            StSInvert -> "sinvert"
            StCayley -> "cayley"
            StPrecond -> "precond"
                                             







-- * FileMode
data PetscFileMode_ = FileModeRead | FileModeWrite | FileModeAppend | FileModeUpdate | FileModeAppendUpdate deriving (Eq, Enum, Show)
fileModeToInt x = fromEnum (x :: PetscFileMode_)

-- FILE_MODE_READ - open a file at its beginning for reading
-- FILE_MODE_WRITE - open a file at its beginning for writing (will create if the file does not exist)
-- FILE_MODE_APPEND - open a file at end for writing
-- FILE_MODE_UPDATE - open a file for updating, meaning for reading and writing
-- FILE_MODE_APPEND_UPDATE - open a file for updating, meaning for reading and writing, at the end




-- * MPI


-- -- Comm
data Comm = Comm {unComm :: CInt} deriving (Eq, Show)
