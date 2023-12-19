-- | Data types for enumerated and non signal unit generator inputs.
module Sound.Sc3.Ugen.Enum where

import Sound.Sc3.Common.Enum {- hsc3 -}
import Sound.Sc3.Common.Envelope {- hsc3 -}
import Sound.Sc3.Ugen.Ugen {- hsc3 -}

-- | Type specialised envelope curve.
type EnvCurve = Envelope_Curve Ugen

-- | Lift to Ugen.
from_buffer :: Buffer Ugen -> Ugen
from_buffer b =
  case b of
    Buffer_Id i -> constant i
    Buffer u -> u
