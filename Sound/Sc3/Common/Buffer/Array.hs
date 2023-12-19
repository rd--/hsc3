{- | Array variants of "Sound.Sc3.Common.Buffer".
These functions have the same names as the plain forms and are not re-exported by "Sound.Sc3.Common".
-}
module Sound.Sc3.Common.Buffer.Array where

import qualified Data.Array as A {- array -}

import qualified Sound.Sc3.Common.Buffer as Common.Buffer {- hsc3 -}

-- | 'Common.Buffer.clipAt'.
clipAt :: Int -> A.Array Int a -> a
clipAt ix c =
  let (l, r) = A.bounds c
      f = (A.!) c
  in if ix < l then f l else if ix > r then f r else f ix

{- | 'C.blendAtBy' of 'clipAt'.

> Sound.Sc3.Common.Buffer.Array.blendAt 0 (A.listArray (0,2) [2,5,6]) == 2
> Sound.Sc3.Common.Buffer.Array.blendAt 0.4 (A.listArray (0,2) [2,5,6]) == 3.2
-}
blendAt :: RealFrac a => a -> A.Array Int a -> a
blendAt = Common.Buffer.blendAtBy clipAt

{- | 'C.resamp1'.

> Sound.Sc3.Common.Buffer.Array.resamp1 12 (A.listArray (0,3) [1,2,3,4])
> Sound.Sc3.Common.Buffer.Array.resamp1 3 (A.listArray (0,3) [1,2,3,4]) == A.listArray (0,2) [1,2.5,4]
-}
resamp1 :: RealFrac n => Int -> A.Array Int n -> A.Array Int n
resamp1 n c =
  let (_, r) = A.bounds c
      gen = Common.Buffer.resamp1_gen n (r + 1) clipAt c
      rs = map gen [0 .. n - 1]
  in A.listArray (0, n - 1) rs
