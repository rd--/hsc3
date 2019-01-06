module Sound.SC3.UGen.M where

import Control.Monad.Trans.State.Lazy {- transformers -}
import Data.Functor.Identity {- base -}

import Sound.SC3.Common.Base {- hsc3 -}
import Sound.SC3.Common.UId {- hsc3 -}
import Sound.SC3.UGen.Rate {- hsc3 -}

import qualified Sound.SC3 as SC3 {- hsc3 -}

newtype UGen m = UGen {mk_ugen :: m SC3.UGen}

type F1 t = t -> t
type F2 t = t -> t -> t
type F3 t = t -> t -> t -> t
type F4 t = t -> t -> t -> t -> t

lift_ugen_1 :: Monad m => F1 SC3.UGen -> F1 (UGen m)
lift_ugen_1 f (UGen p) = UGen $ do
  p' <- p
  return (f p')

lift_ugen_2 :: Monad m => F2 SC3.UGen -> F2 (UGen m)
lift_ugen_2 f (UGen p) (UGen q) = UGen $ do
  p' <- p
  q' <- q
  return (f p' q')

lift_ugen_3 :: Monad m => F3 SC3.UGen -> F3 (UGen m)
lift_ugen_3 f (UGen p) (UGen q) (UGen r) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  return (f p' q' r')

lift_ugen_4 :: Monad m => F4 SC3.UGen -> F4 (UGen m)
lift_ugen_4 f (UGen p) (UGen q) (UGen r) (UGen s) = UGen $ do
  p' <- p
  q' <- q
  r' <- r
  s' <- s
  return (f p' q' r' s')

lift_ugenM_1 :: Monad m => (SC3.UGen -> m SC3.UGen) -> UGen m -> UGen m
lift_ugenM_1 f (UGen p) = UGen $ do
  p' <- p
  f p'

lift_ugenM_2 :: Monad m => (SC3.UGen -> SC3.UGen -> m SC3.UGen) -> UGen m -> UGen m -> UGen m
lift_ugenM_2 f (UGen p) (UGen q) = UGen $ do
  p' <- p
  q' <- q
  f p' q'

instance Monad m => Num (UGen m) where
  (+) = lift_ugen_2 (+)
  (*) = lift_ugen_2 (*)
  (-) = lift_ugen_2 (-)
  abs = lift_ugen_1 abs
  signum = lift_ugen_1 signum
  negate = lift_ugen_1 negate
  fromInteger = UGen . return . fromInteger

instance Monad m => Fractional (UGen m) where
  (/) = lift_ugen_2 (/)
  recip = lift_ugen_1 recip
  fromRational = UGen . return . fromRational

dust :: UId m => Rate -> UGen m -> UGen m
dust rate = lift_ugenM_1 (SC3.dustM rate)

lfNoise1 :: UId m => Rate -> UGen m -> UGen m
lfNoise1 rate = lift_ugenM_1 (SC3.lfNoise1M rate)

rand :: UId m => UGen m -> UGen m -> UGen m
rand = lift_ugenM_2 SC3.randM

whiteNoise :: UId m => Rate -> UGen m
whiteNoise = UGen . SC3.whiteNoiseM

allpassL :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
allpassL = lift_ugen_4 SC3.allpassL

allpassN :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
allpassN = lift_ugen_4 SC3.allpassN

combL :: Monad m => UGen m -> UGen m -> UGen m -> UGen m -> UGen m
combL = lift_ugen_4 SC3.combL

delayN :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
delayN = lift_ugen_3 SC3.delayN

sinOsc :: Monad m => Rate -> UGen m -> UGen m -> UGen m
sinOsc rate = lift_ugen_2 (SC3.sinOsc rate)

out :: Monad m => UGen m -> UGen m -> UGen m
out = lift_ugen_2 SC3.out

mce2 :: Monad m => UGen m -> UGen m -> UGen m
mce2 = lift_ugen_2 SC3.mce2

resonz :: Monad m => UGen m -> UGen m -> UGen m -> UGen m
resonz = lift_ugen_3 SC3.resonz

{-
import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}

draw :: UGen (StateT Int Identity) -> IO ()
draw (UGen u) = Dot.draw (uid_st_eval u)
-}

audition :: UGen (StateT Int Identity) -> IO ()
audition (UGen u) = SC3.audition (uid_st_eval u)

-- > draw t0
-- > audition t0
t0 :: UId m => UGen m
t0 = out 0 (mce2 (sinOsc AR 440 0 * 0.1) ((whiteNoise AR - whiteNoise AR) * 0.1))

-- > draw why_supercollider
-- > audition why_supercollider
why_supercollider :: UId m => UGen m
why_supercollider =
    let r = resonz (dust AR 0.2 * 50) (rand 200 3200) 0.003
        s = sum (replicate 10 r)
        c = combL (delayN s 0.048 0.048) 0.1 (lfNoise1 KR (rand 0 0.1) * 0.04 + 0.05) 15
        y = sum (replicate 7 c)
        f i = allpassN i 0.05 (mce2 (rand 0 0.05) (rand 0 0.05)) 1
        x = compose_l (replicate 4 f) y
    in s + 0.2 * x
