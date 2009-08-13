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
