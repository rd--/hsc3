module Sound.SC3.UGen.Envelope where

import Sound.SC3.UGen.UGen (UGen(..), mkOsc, mkFilter)
import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.Math ()

data DoneAction = DoNothing
                | PauseSynth
                | RemoveSynth
                | DoneAction UGen
                  deriving (Eq, Show)

fromAction :: DoneAction -> UGen
fromAction DoNothing      = Constant 0
fromAction PauseSynth     = Constant 1
fromAction RemoveSynth    = Constant 2
fromAction (DoneAction u) = u

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

-- UGens...

envGen :: Rate -> UGen -> UGen -> UGen -> UGen -> DoneAction -> [UGen] -> UGen
envGen r gate lvl bias scale done pts = mkOsc r "EnvGen" i 1 0
 where i = [gate, lvl, bias, scale, fromAction done] ++ pts

line :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
line r start end dur done = mkOsc r "Line" [start, end, dur, fromAction done] 1 0

xLine :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
xLine r start end dur done = mkOsc r "XLine" [start, end, dur, fromAction done] 1 0

freeSelf :: UGen -> UGen
freeSelf i = mkFilter "FreeSelf" [i] 0 0

pauseSelf :: UGen -> UGen
pauseSelf i = mkFilter "PauseSelf" [i] 0 0
