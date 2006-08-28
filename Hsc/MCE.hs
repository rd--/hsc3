module Hsc.MCE where

import Hsc.UGen (UGen(..), isMCE)
import Data.List (transpose)

mceDegree :: UGen -> Int
mceDegree (MCE l) = length l
mceDegree _       = error "mceDegree: illegal ugen"

mceReq :: UGen -> Bool
mceReq (UGen _ _ i _ _ _) = not $ null $ filter isMCE i
mceReq _                  = False

mceExtend :: Int -> UGen -> [UGen]
mceExtend n (MCE l) = take n $ cycle l
mceExtend n u       = replicate n u

mceTransform :: UGen -> UGen
mceTransform (UGen r n i o s uid) = MCE (map f i')
    where f j = UGen r n j o s uid
          d   = maximum $ map mceDegree (filter isMCE i)
          i'  = transpose $ map (mceExtend d) i
mceTransform _                   = error "mceTransform: illegal ugen"

mced :: UGen -> UGen
mced u = if mceReq u then mceTransform u else u
