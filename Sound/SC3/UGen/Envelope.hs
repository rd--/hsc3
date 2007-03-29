module Sound.SC3.UGen.Envelope where

import Sound.SC3.UGen.UGen (UGen(..), mkOsc, mkFilter)
import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.Math ((>=*), (<=*))
import Sound.SC3.UGen.Enum (DoneAction, fromDoneAction)

data EnvCurve = EnvStep
              | EnvLin | EnvExp
              | EnvSin | EnvCos
              | EnvNum UGen
              | EnvSqr | EnvCub
              deriving (Eq, Show)

env_curve :: EnvCurve -> UGen
env_curve EnvStep    = 0.0
env_curve EnvLin     = 1.0
env_curve EnvExp     = 2.0
env_curve EnvSin     = 3.0
env_curve EnvCos     = 4.0
env_curve (EnvNum _) = 5.0
env_curve EnvSqr     = 6.0
env_curve EnvCub     = 7.0

env_value :: EnvCurve -> UGen
env_value (EnvNum u) = u
env_value _          = 0.0

env :: [UGen] -> [UGen] -> [EnvCurve] -> UGen -> UGen -> [UGen]
env []     _   _   _   _  = error "env: illegal specification"
env (l:vl) tms crv rls lp =
    [l, n', rls, lp] ++ concat (zipWith3 f vl tms (take n $ cycle crv))
    where f l' t c = [l', t, env_curve c, env_value c]
          n       = length tms
          n'      = fromIntegral n

d_dx :: (Num a) => [a] -> [a]
d_dx [] = []
d_dx [_] = []
d_dx [x,y] = [y - x]
d_dx (x:y:r) = y - x : d_dx (y:r)

-- | Co-ordinate based static envelope generator.

envCoord :: [(UGen, UGen)] -> UGen -> UGen -> EnvCurve -> [UGen]
envCoord bp dur amp c = env l t (repeat c) (-1) (-1)
    where l = map (* amp) (map snd bp)
          t = map (* dur) (d_dx (map fst bp))

-- | Trapezoidal envelope generator.  `shape' determines the sustain
-- | time as a proportion of `dur', zero is a triangular envelope, one
-- | a rectangular envelope.  `skew' determines the attack/decay
-- | ratio, zero is an immediate attack and a slow decay, one a slow
-- | attack and an immediate decay.

envTrapezoid :: UGen -> UGen -> UGen -> UGen -> [UGen]
envTrapezoid shape skew dur amp = envCoord bp dur amp EnvLin
    where x1 = skew * (1 - shape)
          bp = [ (0, skew <=* 0)
               , (x1, 1)
               , (shape + x1, 1)
               , (1, skew >=* 1) ]

envPerc :: UGen -> UGen -> UGen -> [EnvCurve] -> [UGen]
envPerc atk rls lvl crv = env [0.0, lvl, 0.0] [atk, rls] crv (-1.0) (-1.0)

envPerc' :: [UGen]
envPerc' = envPerc 0.01 1.0 1.0 (dbl (EnvNum (-4.0)))

envTriangle :: UGen -> UGen -> [UGen]
envTriangle dur lvl =
   env [0.0, lvl, 0.0] (dbl (dur / 2.0)) (dbl EnvLin) (-1.0) (-1.0)

envSine :: UGen -> UGen -> [UGen]
envSine dur lvl =
   env [0.0, lvl, 0.0] (dbl (dur / 2.0)) (dbl EnvSin) (-1.0) (-1.0)

envLinen :: UGen -> UGen -> UGen -> UGen -> [EnvCurve] -> [UGen]
envLinen aT sT rT l c = env [0, l, l, 0] [aT, sT, rT] c (-1) (-1)

dbl :: a -> [a]
dbl x = [x,x]

-- | Segment based envelope generator.
envGen :: Rate -> UGen -> UGen -> UGen -> UGen -> DoneAction -> [UGen] -> UGen
envGen r gate lvl bias scale act pts = mkOsc r "EnvGen" i 1 0
 where i = [gate, lvl, bias, scale, fromDoneAction act] ++ pts

-- | Line generator.
line :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
line r start end dur act = mkOsc r "Line" [start, end, dur, fromDoneAction act] 1 0

-- | Exponential line generator.
xLine :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
xLine r start end dur act = mkOsc r "XLine" [start, end, dur, fromDoneAction act] 1 0

-- | Free node on trigger.
freeSelf :: UGen -> UGen
freeSelf i = mkFilter "FreeSelf" [i] 0 0

-- | Free node on done action at source.
freeSelfWhenDone :: UGen -> UGen
freeSelfWhenDone i = mkFilter "FreeSelfWhenDone" [i] 0 0

-- | Pause specified node on trigger.
pause :: UGen -> UGen -> UGen
pause t n = mkFilter "Pause" [t, n] 1 0

-- | Pause node on trigger.
pauseSelf :: UGen -> UGen
pauseSelf i = mkFilter "PauseSelf" [i] 0 0

-- | Pause node on done action at source.
pauseSelfWhenDone :: UGen -> UGen
pauseSelfWhenDone i = mkFilter "PauseSelfWhenDone" [i] 0 0

-- | One while the source is marked done, else zero.
done :: UGen -> UGen
done i = mkFilter "Done" [i] 1 0

-- | Raise specified done action when input goes silent.
detectSilence ::  UGen -> UGen -> UGen -> DoneAction -> UGen
detectSilence i a t act = mkFilter "DetectSilence" [i, a, t, fromDoneAction act] 0 0

-- | When triggered free specified node.
free :: UGen -> UGen -> UGen
free i n = mkFilter "Free" [i, n] 1 0

-- | Linear envelope generator.
linen :: UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
linen g at sl rt da = mkFilter "Linen" [g,at,sl,rt,fromDoneAction da] 1 0

