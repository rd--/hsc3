-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.UGen.Enum where

import Sound.SC3.Common.Envelope {- hsc3 -}
import Sound.SC3.Common.Enum {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}

-- | Type specialised envelope curve.
type EnvCurve = Envelope_Curve UGen

-- | Lift to UGen.
from_buffer :: Buffer UGen -> UGen
from_buffer b =
    case b of
      Buffer_Id i -> constant i
      Buffer u -> u
