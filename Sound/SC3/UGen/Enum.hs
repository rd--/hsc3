-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.UGen.Enum where

import qualified Sound.SC3.Common.Envelope as E
import Sound.SC3.UGen.Type

-- | Loop indicator input.
data Loop' t = Loop
             | NoLoop
             | WithLoop t
               deriving (Eq, Show)

-- | Type-specialised 'Loop''.
type Loop = Loop' UGen

-- | Resolve 'Loop''.
from_loop :: Num t => Loop' t -> t
from_loop e =
    case e of
      NoLoop -> 0
      Loop -> 1
      WithLoop u -> u

-- | Interpolation indicator input.
data Interpolation = NoInterpolation
                   | LinearInterpolation
                   | CubicInterpolation
                   | Interpolation UGen
                     deriving (Eq, Show)

-- | Resolve 'Interpolation'.
from_interpolation :: Interpolation -> UGen
from_interpolation e =
    case e of
      NoInterpolation -> 1
      LinearInterpolation -> 2
      CubicInterpolation -> 4
      Interpolation u -> u

-- | Completion mode indicator input.
data DoneAction = DoNothing
                | PauseSynth
                | RemoveSynth
                | RemoveGroup
                | DoneAction UGen
                  deriving (Eq, Show)

-- | Resolve 'DoneAction'.
from_done_action :: DoneAction -> UGen
from_done_action e =
    case e of
      DoNothing -> 0
      PauseSynth -> 1
      RemoveSynth -> 2
      RemoveGroup -> 14
      DoneAction u -> u

-- | Warp interpolation indicator input.
data Warp = Linear
          | Exponential
          | Warp UGen
            deriving (Eq, Show)

-- | Resolve 'Warp'.
from_warp :: Warp -> UGen
from_warp e =
    case e of
      Linear -> 0
      Exponential -> 1
      Warp u -> u

-- | Type specialised ('UGen') envelope curve.
type EnvCurve = E.Envelope_Curve UGen

-- | Unification of integer and 'UGen' buffer identifiers.
data Buffer = Buffer_Id Int
            | Buffer UGen
              deriving (Eq, Show)

-- | Lift to 'UGen'.
from_buffer :: Buffer -> UGen
from_buffer b =
    case b of
      Buffer_Id i -> constant i
      Buffer u -> u
