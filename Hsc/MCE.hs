module Hsc.MCE where

import Hsc.UGen
import Data.List (transpose)

mceDegree :: UGen -> Int
mceDegree (MCE l) = length l

mceReq :: UGen -> Bool
mceReq (UGen _ _ i _ _ _) = not $ null $ filter isMCE i
mceReq _                  = False

mceExtend :: Int -> UGen -> [UGen]
mceExtend n (MCE l) = take n $ cycle l
mceExtend n u       = replicate n u

mceTransform :: UGen -> UGen
mceTransform (UGen r n i o s id) = MCE (map f i')
    where f i = UGen r n i o s id
          d   = maximum $ map mceDegree (filter isMCE i)
          i'  = transpose $ map (mceExtend d) i

mced :: UGen -> UGen
mced u = if mceReq u then mceTransform u else u
