module Hsc.MCE where

import Hsc.UGen
import Hsc.List (invert)

mceDegree (MCE l) = length l

mceRef (MCE l) n = l !! n

--mceReq :: UGen -> Bool
mceReq (UGen _ _ i _ _ _) = not $ null $ filter isMCE i
mceReq _                  = False

-- To be mapped over the inputs list.

--mceExtend :: Int -> UGen -> [UGen]
mceExtend n (MCE l) = take n $ cycle l
mceExtend n u       = replicate n u

--mce :: UGen -> UGen
mceTransform (UGen r n i o s id) = MCE (map f i')
    where f i = UGen r n i o s id
          d   = maximum $ map mceDegree (filter isMCE i)
          i'  = invert $ map (mceExtend d) i

mced u = if mceReq u then mceTransform u else u

{--
mcel :: UGen -> [UGen]
mcel (MCE l) = l
mcel u       = [u]

-- Out and such like use this to force any required MCE.

forceMCE :: UGen -> [UGen]
forceMCE u = mcel $ traverseu f u
    where f u = if reqMCE u then mce u else u
--}

