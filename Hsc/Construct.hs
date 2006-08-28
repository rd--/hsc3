module Hsc.Construct where

import Hsc.UGen (UId(..), UGen(..), Special, Name, Output, rateOf, proxy)
import Hsc.MCE (mced)
import Hsc.Rate (Rate)

import Data.Unique (newUnique, hashUnique)
import Control.Monad (liftM, replicateM)

zeroUId :: UId
zeroUId = UId 0

mkId :: IO Int
mkId = liftM hashUnique newUnique

mkUId :: IO UId
mkUId = liftM UId mkId

uniquify :: UGen -> IO UGen
uniquify (UGen r n i o s _) = liftM (UGen r n i o s) mkUId
uniquify (MCE u)            = liftM MCE (mapM uniquify u)
uniquify u                  = error ("uniquify: illegal value" ++ show u)

consU :: Rate -> Name -> [UGen] -> [Output] -> Special -> UId -> UGen
consU r n i o s uid = proxy (mced u)
    where u = UGen r n i o s uid

mkOsc :: Rate -> Name -> [UGen] -> Int -> Special -> UGen
mkOsc r c i o s = consU r c i o' s zeroUId
    where o' = replicate o r

mkOsc' :: Rate -> Name -> [UGen] -> Int -> Special -> UId -> UGen
mkOsc' r c i o s uid = consU r c i o' s uid
    where o' = replicate o r

mkFilter :: Name -> [UGen] -> Int -> Special -> UGen
mkFilter c i o s = consU r c i o' s zeroUId
    where r = maximum (map rateOf i)
          o'= replicate o r

mcel :: UGen -> [UGen]
mcel (MCE l) = l
mcel u       = [u]

mkOscMCE :: Rate -> Name -> [UGen] -> UGen -> Int -> Special -> UGen
mkOscMCE r c i j o s = mkOsc r c (i ++ mcel j) o s

mkFilterMCE :: Name -> [UGen] -> UGen -> Int -> Special -> UGen
mkFilterMCE c i j o s = mkFilter c (i ++ mcel j) o s

--

dupn :: Int -> UGen -> IO UGen
dupn  n u = liftM MCE (replicateM n (uniquify u))

dupn' :: Int -> IO UGen -> IO UGen
dupn' n u = u >>= dupn n

dup :: UGen -> IO UGen
dup       = dupn 2

dup' :: IO UGen -> IO UGen
dup'      = dupn' 2
