module Hsc.Envelope where

import Hsc.UGen
import Hsc.Math
import Hsc.Construct

envgen r gate lvl bias scale done env = mkOsc r "EnvGen" i 1 0 r0
    where i = [gate,lvl,bias,scale,done] ++ env

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
env (l:vl) tms crv rls lp =
    [l, n', rls, lp] ++ concat (zipWith3 f vl tms (take n $ cycle crv))
    where f l t c = [l, t, env_curve c, env_value c]
          n       = length tms
          n'      = fromIntegral n

envperc :: UGen -> UGen -> UGen -> [EnvCurve] -> [UGen]
envperc atk rls lvl crv = env [0.0, lvl, 0.0] [atk, rls] crv (-1.0) (-1.0)

envperc' :: [UGen]
envperc' = envperc 0.01 1.0 1.0[c,c]
    where c = EnvNum (-4.0)

envtriangle :: UGen -> UGen -> [UGen]
envtriangle dur lvl = env [0.0, lvl, 0.0] [n, n] c (-1.0) (-1.0)
    where c = [EnvLin, EnvLin]
          n = dur / 2.0

envsine :: UGen -> UGen -> [UGen]
envsine dur lvl = env [0.0, lvl, 0.0] [n, n] c (-1.0) (-1.0)
    where c = [EnvSin, EnvSin]
          n = dur / 2.0

xline r start end dur done = mkOsc r "XLine" [start,end,dur,done] 1 0 r0
line  r start end dur done = mkOsc r "Line"  [start,end,dur,done] 1 0 r0

freeself  i = mkFilter "FreeSelf"  [i] 1 0 r0
pauseself i = mkFilter "PauseSelf" [i] 1 0 r0
