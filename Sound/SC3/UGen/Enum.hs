-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.UGen.Enum where

import Sound.SC3.Common.Envelope
import Sound.SC3.Common.Enum
import Sound.SC3.UGen.Type

-- | Type specialised ('UGen') envelope curve.
type EnvCurve = Envelope_Curve UGen

-- | Lift to 'UGen'.
from_buffer :: Buffer UGen -> UGen
from_buffer b =
    case b of
      Buffer_Id i -> constant i
      Buffer u -> u
