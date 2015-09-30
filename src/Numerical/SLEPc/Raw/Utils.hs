module Numerical.SLEPc.Raw.Utils where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Control.Monad
import Control.Arrow ((***),(&&&))






withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings ss f = case ss of
  [] -> f []
  (s:ss') -> withCString s $ \cs -> 
    withCStrings ss' $ \css -> f (cs:css)

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray ss f = withCStrings ss $ \css -> withArray css f

withCStringArrayPtr :: [String] -> (Ptr (Ptr CString) -> IO a) -> IO a
withCStringArrayPtr ss f = withCStringArray ss $ \css -> with css f



-- -- tuple unpacking stuff

-- fst2 :: (a, (b, c)) -> (a,b)
fst2 = fst . snd
-- snd2 :: (a, (b, c)) -> c
snd2 =  snd . snd

both' f =  f *** f

both (a, b) f = (f a, f b)

bothF (a, b) f = (fmap f a, fmap f b)

all3 (a,b,c) f = (f a, f b, f c)

bothM t f = return (both t f)

(_1) f (a, b) =
  f a >>= \x -> return (x, b) -- a special case of _1 from Control.Lens

(_2) f (a, b) =
  f b >>= \y -> return (a, y) -- _2, "


-- * Int, CInt stuff

fromIntegralTup t = both t fi
fromIntegralTup2 t = both t fromIntegralTup
fromIntegralTup3 t = both t (`all3` fi)


fi :: CInt -> Int
fi = fromIntegral

toCInt :: Int -> CInt
toCInt = CInt . fromIntegral
