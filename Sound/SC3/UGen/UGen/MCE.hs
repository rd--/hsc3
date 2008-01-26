module Sound.SC3.UGen.UGen.MCE where

import Data.List (transpose)
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Predicate

-- * Multiple Channel Expansion

-- | Number of channels to expand to.
mceDegree :: UGen -> Int
mceDegree (MCE l) = length l
mceDegree (MRG u _) = mceDegree u
mceDegree _ = error "mceDegree: illegal ugen"

-- | Extend UGen to specified degree.
mceExtend :: Int -> UGen -> [UGen]
mceExtend n (MCE l) = take n (cycle l)
mceExtend n (MRG x y) = (MRG r y : rs) where (r:rs) = mceExtend n x
mceExtend n u = replicate n u

-- | Apply MCE transformation.
mceTransform :: UGen -> UGen
mceTransform (Primitive r n i o s d) = MCE (map f i')
    where f j = Primitive r n j o s d
          upr = maximum (map mceDegree (filter isMCE i))
          i' = transpose (map (mceExtend upr) i)
mceTransform _ = error "mceTransform: illegal ugen"

-- | Apply MCE transformation if required.
mceExpand :: UGen -> UGen
mceExpand (MCE l) = MCE (map mceExpand l)
mceExpand (MRG x y) = MRG (mceExpand x) y
mceExpand u = if required u then mceExpand (mceTransform u) else u
    where required (Primitive _ _ i _ _ _) = not (null (filter isMCE i))
          required (MCE l) = any required l
          required _ = False

-- | Apply UGen list operation on MCE contents.
mceEdit :: ([UGen] -> [UGen]) -> UGen -> UGen
mceEdit f (MCE l) = MCE (f l)
mceEdit _ _ = error "mceEdit: non MCE value"

-- | Reverse order of channels at MCE.
mceReverse :: UGen -> UGen
mceReverse = mceEdit reverse

-- | Obtain indexed channel at MCE.
mceChannel :: Int -> UGen -> UGen
mceChannel n (MCE l) = l !! n
mceChannel _ _ = error "mceChannel: non MCE value"

-- | Output channels of UGen as a list.
mceChannels :: UGen -> [UGen]
mceChannels (MCE l) = l
mceChannels (MRG x y) = (MRG r y) : rs where (r:rs) = mceChannels x
mceChannels u = [u]

mceInterleave :: UGen -> UGen -> UGen
mceInterleave x y = mce (zipWith mce2 (mceChannels x) (mceChannels y))
