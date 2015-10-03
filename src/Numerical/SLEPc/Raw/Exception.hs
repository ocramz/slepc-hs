{-# LANGUAGE DeriveDataTypeable #-}
module Numerical.SLEPc.Raw.Exception where
-- | Exception handling for calls wrapped by PutGet interfaces

import Data.Maybe
import Data.Functor
import Data.Typeable

import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

import Foreign.C.Types


slepcErrCodeFromInt n =
  case n of 0 -> NoError
            256 -> NoError  -- w t f does this work
            _ -> SomeError 

data SlepcErrCode_ = NoError | SomeError  deriving (Eq, Show, Typeable)

instance Exception SlepcErrCode_

chk1 act = do
  (v, e) <- act
  let ec = slepcErrCodeFromInt e
  case ec of NoError -> return v
             e'       -> throwIO e'

chk0 act = do
  e <- act
  let ec = slepcErrCodeFromInt e
  case ec of NoError -> return ()
             e'       -> throwIO e'

bracketChk alpha omega = bracket (chk1 alpha) (chk0 . omega)

-- -- --

-- slepcErrCodeFromInt n =
--   case n of 0 -> NoError
--             x -> SomeError x

-- chk1 act =
--   catch act $ \e ->
--    case e of 0 -> return ()


-- data SlepcErrCode_ = NoError | SomeError Int deriving (Eq, Show, Typeable)

-- instance Exception SlepcErrCode_ 

-- chk1 act = do
--   (v, e) <- act
--   let ec = slepcErrCodeFromInt e
--   case ec of NoError          -> return v
--              e'@(SomeError x) -> throwIO e'

-- chk0 act = do
--   e <- act
--   let ec = slepcErrCodeFromInt e
--   case ec of NoError -> return ()
--              e'       -> throwIO e'

-- bracketChk alpha omega = bracket (chk1 alpha) (chk0 . omega)


