-- | Internal UGen related functions.
module Sound.SC3.UGen.Utilities where

import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.UGen

-- * Un-enumerations.

-- | Resolve 'Loop'.
from_loop :: Loop -> UGen
from_loop e =
    case e of
      NoLoop -> Constant 0
      Loop -> Constant 1
      WithLoop u -> u

-- | Resolve 'Interpolation'.
from_interpolation :: Interpolation -> UGen
from_interpolation e =
    case e of
      NoInterpolation -> Constant 1
      LinearInterpolation -> Constant 2
      CubicInterpolation -> Constant 4
      Interpolation u -> u

-- | Resolve 'DoneAction'.
from_done_action :: DoneAction -> UGen
from_done_action e =
    case e of
      DoNothing -> Constant 0
      PauseSynth -> Constant 1
      RemoveSynth -> Constant 2
      DoneAction u -> u

-- | Resolve 'Warp'.
from_warp :: Warp -> UGen
from_warp e =
    case e of
      Linear -> Constant 0
      Exponential -> Constant 1
      Warp u -> u
