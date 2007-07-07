module Sound.SC3.UGen.UGen.MCE where

import Data.List (transpose)
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Predicate

-- * Multiple Channel Expansion

-- | Number of channels to expand to.
mceDegree :: UGen -> Int
mceDegree (UGen _ _ _ _ _ _) = 1
mceDegree (MCE l) = length l
mceDegree _       = error "mceDegree: illegal ugen"

-- | Is expansion required, ie. are any inputs MCE values.
mceRequired :: UGen -> Bool
mceRequired (UGen _ _ i _ _ _) = not (null (filter isMCE i))
mceRequired (MCE l)            = any mceRequired l
mceRequired _                  = False

-- | Extend UGen to specified degree.
mceExtend :: Int -> UGen -> [UGen]
mceExtend n (MCE l) = take n (cycle l)
mceExtend n u       = replicate n u

-- | Apply MCE transformation.
mceTransform :: UGen -> UGen
mceTransform (UGen r n i o s d) = MCE (map f i')
    where f j = UGen r n j o s d
          upr = maximum (map mceDegree (filter isMCE i))
          i'  = transpose (map (mceExtend upr) i)
mceTransform _ = error "mceTransform: illegal ugen"

-- | Apply MCE transformation if required.
mceExpand :: UGen -> UGen
mceExpand (MCE l) = MCE (map mceExpand l)
mceExpand u       = if mceRequired u then mceExpand (mceTransform u) else u

-- | Apply UGen list operation on MCE contents.
mceEdit f (MCE l) = MCE (f l)
mceEdit _ _ = error "mceEdit: non MCE value"

-- | Reverse order of channels at MCE.
mceReverse :: UGen -> UGen
mceReverse = mceEdit reverse

-- | Obtain indexed channel at MCE.
mceChannel :: Int -> UGen -> UGen
mceChannel n (MCE l) = l !! n
mceChannel _ _       = error "mceChannel: non MCE value"

-- | Output channels of UGen as a list.
mceChannels :: UGen -> [UGen]
mceChannels (MCE l) = l
mceChannels u       = [u]

