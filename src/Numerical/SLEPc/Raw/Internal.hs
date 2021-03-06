{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
module Numerical.SLEPc.Raw.Internal
       where

-- import Numerical.PETSc.Types
  --(PetscInt, PetscBool, PetscReal, PetscScalar)

import qualified Language.C.Types as CT
import Foreign.C.Types
import Foreign
import qualified Language.Haskell.TH as TH
import Language.C.Inline
import Language.C.Inline.Context
import Data.Monoid ((<>), mempty)
import Control.Monad (unless, when, liftM)
import qualified Data.Map as Map


-- * inline-c SLEPc Context (type maps)
slepcCtx :: Context
slepcCtx = baseCtx <> funCtx <> vecCtx <> ctx where
  ctx = mempty {ctxTypesTable = slepcTypesTable}

slepcTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ
slepcTypesTable = Map.fromList
                  [
                    (typeNameId "PetscInt", [t| PetscInt_ |] )
                  , (typeNameId "PetscReal", [t| PetscReal_ |])  
                  , (typeNameId "PetscMPIInt", [t| PetscMPIInt_ |] )
                  , (typeNameId "PetscError", [t| PetscError_ |]) 
                  , (typeNameId "PetscBool", [t| PetscBool_ |])
                  , (typeNameId "PetscScalar", [t| PetscScalar_ |])
                  , (typeNameId "PetscLogStage", [t| PetscLogStage_ |])  

                  , (typeNameId "IS", [t| IS |] )
                  , (typeNameId "Vec", [t| Vec |] )
                  , (typeNameId "Mat", [t| Mat |] )
                    
                  , (typeNameId "EPS", [t| EPS |])
                  , (typeNameId "SVD", [t| SVD |])
                  , (typeNameId "PEP", [t| PEP |])
                  , (typeNameId "NEP", [t| NEP |])
                  , (typeNameId "MFN", [t| MFN |])                    

                  , (typeNameId "ST", [t| ST |])
                  , (typeNameId "DS", [t| DS |])
                  , (typeNameId "BV", [t| BV |])
                  , (typeNameId "FN", [t| FN |])
                  , (typeNameId "RG", [t| RG |])  
                    
                  -- , (typeNameId "KSP", [t| KSP |])

                  , (typeNameId "PetscViewer", [t| PetscViewer |])
                  ]

typeNameId :: String -> CT.TypeSpecifier
typeNameId = CT.TypeName . CT.Identifier

-- * type synonyms

type PetscLogStage_ = CInt
type PetscError_ = CInt

type PetscInt_ = CInt
type PetscBool_ = Bool
type PetscScalar_ = CDouble
type PetscReal_ = CDouble

type MatConst = CInt


-- -- FIXME : robust approach would be to infer the Hs types with c2hs

-- type PetscInt_ = PetscInt
-- type PetscBool_ = PetscBool
-- type PetscScalar_ = PetscScalar
-- type PetscReal_ = PetscReal



-- * newtypes

newtype PetscMPIInt_ = PetscMPIInt_ (Ptr PetscMPIInt_ ) deriving (Show, Storable)

newtype PetscReal = PetscReal (Ptr PetscReal)
instance Storable PetscReal where
  sizeOf _ = sizeOf (undefined :: PetscReal_)
  alignment = sizeOf
  peek = peek
  poke = poke 
 -- peek p = do i <- (# peek Foo, i) p
 --                j <- (# peek Foo, j) p
 --                k <- (# peek Foo, k) p
 --                return $ Foo i j k
 --    poke p foo = do (# poke Foo, i) p (i foo)
 --                    (# poke Foo, j) p (j foo)
 --                    (# poke Foo, k) p (k foo)


newtype IS = IS (Ptr IS) deriving Storable

newtype Vec = Vec (Ptr Vec) deriving Storable

newtype Mat = Mat (Ptr Mat) deriving Storable

newtype EPS = EPS (Ptr EPS) deriving Storable

newtype SVD = SVD (Ptr SVD) deriving Storable

newtype PEP = PEP (Ptr PEP) deriving Storable

newtype NEP = NEP (Ptr NEP) deriving Storable

newtype MFN = MFN (Ptr MFN) deriving Storable


-- Spectral Transformation (ST) 
newtype ST = ST (Ptr ST) deriving Storable
-- Direct Solver (DS)
newtype DS = DS (Ptr DS) deriving Storable
-- Basis Vectors (BV)
newtype BV = BV (Ptr BV) deriving Storable
-- Mathematical Function (FN)
newtype FN = FN (Ptr FN) deriving Storable
-- spectral Region (RG)
newtype RG = RG (Ptr RG) deriving Storable


newtype PetscViewer = PetscViewer (Ptr PetscViewer) deriving Storable
