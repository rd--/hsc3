module Sound.SC3.UGen.Utilities where

import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.UGen

from_loop :: Loop -> UGen
from_loop NoLoop = Constant 0
from_loop Loop = Constant 1
from_loop (WithLoop u) = u

from_interpolation :: Interpolation -> UGen
from_interpolation NoInterpolation = Constant 1
from_interpolation LinearInterpolation = Constant 2
from_interpolation CubicInterpolation = Constant 4
from_interpolation (Interpolation u) = u

from_done_action :: DoneAction -> UGen
from_done_action DoNothing = Constant 0
from_done_action PauseSynth = Constant 1
from_done_action RemoveSynth = Constant 2
from_done_action (DoneAction u) = u

from_warp :: Warp -> UGen
from_warp Linear = Constant 0
from_warp Exponential = Constant 1
from_warp (Warp u) = u

env_curve :: EnvCurve -> UGen
env_curve EnvStep = Constant 0.0
env_curve EnvLin = Constant 1.0
env_curve EnvExp = Constant 2.0 
env_curve EnvSin = Constant 3.0
env_curve EnvCos = Constant 4.0
env_curve (EnvNum _) = Constant 5.0
env_curve EnvSqr = Constant 6.0
env_curve EnvCub = Constant 7.0

env_value :: EnvCurve -> UGen
env_value (EnvNum u) = u
env_value _ = Constant 0.0

d_dx :: (Num a) => [a] -> [a]
d_dx [] = []
d_dx [_] = []
d_dx [x,y] = [y - x]
d_dx (x:y:r) = y - x : d_dx (y:r)

dbl :: a -> [a]
dbl x = [x, x]
