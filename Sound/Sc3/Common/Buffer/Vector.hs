{- | Vector variants of "Sound.Sc3.Common.Buffer".
These functions have the same names as the plain forms and are not re-exported by "Sound.Sc3.Common".
-}
module Sound.Sc3.Common.Buffer.Vector where

import qualified Data.Vector.Storable as V {- vector -}

import qualified Sound.Sc3.Common.Buffer as C {- hsc3 -}

-- | 'C.clipAt'.
clipAt :: V.Storable t => Int -> V.Vector t -> t
clipAt ix c =
  let r = V.length c
      f = (V.!) c
  in if ix > r - 1 then f (r - 1) else f ix

{- | 'C.blendAtBy' of 'clipAt'.

> Sound.Sc3.Common.Buffer.Vector.blendAt 0 (V.fromList [2,5,6]) == 2
> Sound.Sc3.Common.Buffer.Vector.blendAt 0.4 (V.fromList [2,5,6]) == 3.2
> Sound.Sc3.Common.Buffer.Vector.blendAt 2.1 (V.fromList [2,5,6]) == 6
-}
blendAt :: (V.Storable t, RealFrac t) => t -> V.Vector t -> t
blendAt = C.blendAtBy clipAt

{- | 'C.from_wavetable'

> Sound.Sc3.Common.Buffer.Vector.from_wavetable (V.fromList [-0.5,0.5,0,0.5,1.5,-0.5,1,-0.5])
-}
from_wavetable :: (V.Storable t, Num t) => V.Vector t -> V.Vector t
from_wavetable wt =
  let n = V.length wt
      f k = let k2 = k * 2 in (wt V.! k2) + (wt V.! (k2 + 1))
  in V.generate (n `div` 2) f

{- | 'C.resamp1'.

> Sound.Sc3.Common.Buffer.Vector.resamp1 12 (V.fromList [1,2,3,4])
> Sound.Sc3.Common.Buffer.Vector.resamp1 3 (V.fromList [1,2,3,4]) == V.fromList [1,2.5,4]
-}
resamp1 :: (V.Storable t, RealFrac t) => Int -> V.Vector t -> V.Vector t
resamp1 n c =
  let gen = C.resamp1_gen n (V.length c) clipAt c
  in V.generate n gen
