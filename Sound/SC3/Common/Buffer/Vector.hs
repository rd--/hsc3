-- | 'V.Vector' variants of "Sound.SC3.Common.Buffer".
module Sound.SC3.Common.Buffer.Vector where

import qualified Data.Vector as V {- vector -}

import qualified Sound.SC3.Common.Buffer as C {- hsc3 -}

-- | 'C.clipAt'.
clipAt :: Int -> V.Vector a -> a
clipAt ix c =
    let r = V.length c
        f = (V.!) c
    in if ix > r - 1 then f (r - 1) else f ix

-- | 'C.blendAtBy' of 'clipAt'.
--
-- > blendAt 0 (V.fromList [2,5,6]) == 2
-- > blendAt 0.4 (V.fromList [2,5,6]) == 3.2
-- > blendAt 2.1 (V.fromList [2,5,6]) == 6
blendAt :: RealFrac a => a -> V.Vector a -> a
blendAt = C.blendAtBy clipAt

-- | 'C.resamp1'.
--
-- > resamp1 12 (V.fromList [1,2,3,4])
-- > resamp1 3 (V.fromList [1,2,3,4]) == V.fromList [1,2.5,4]
resamp1 :: RealFrac n => Int -> V.Vector n -> V.Vector n
resamp1 n c =
    let gen = C.resamp1_gen n (V.length c) clipAt c
    in V.generate n gen
