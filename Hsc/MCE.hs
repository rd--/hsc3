module Hsc.MCE where

import Hsc.UGen
import Hsc.List (invert)

mcel :: UGen -> [UGen]
mcel (MCE l) = l
mcel u       = [u]

reqMCE :: UGen -> Bool
reqMCE (UGen _ _ i _ _ _) = not $ null $ filter isMCE i
reqMCE _                  = False

mceDepth :: [UGen] -> Int
mceDepth i = maximum $ map (length . mcel) $ filter isMCE i

-- To be mapped over the inputs list.

extendu :: Int -> UGen -> [UGen]
extendu n (MCE l) = take n $ cycle l
extendu n u       = replicate n u

mce :: UGen -> UGen
mce (UGen r n i o s id) = MCE (map f i')
    where f i = UGen r n i o s id
          d   = mceDepth i
          i'  = invert $ map (extendu d) i

-- Out and such like use this to force any required MCE.

forceMCE :: UGen -> [UGen]
forceMCE u = mcel $ traverseu f u
    where f u = if reqMCE u then mce u else u
