-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.Common.Enum where

-- | Loop indicator input.
data Loop t =
    Loop
  | NoLoop
  | WithLoop t
  deriving (Eq, Show)

-- | Resolve 'Loop'.
from_loop :: Num t => Loop t -> t
from_loop e =
    case e of
      NoLoop -> 0
      Loop -> 1
      WithLoop u -> u

-- | Interpolation indicator input.
data Interpolation t =
    NoInterpolation
  | LinearInterpolation
  | CubicInterpolation
  | Interpolation t
  deriving (Eq, Show)

-- | Resolve 'Interpolation'.
from_interpolation :: Num t => Interpolation t -> t
from_interpolation e =
    case e of
      NoInterpolation -> 1
      LinearInterpolation -> 2
      CubicInterpolation -> 4
      Interpolation u -> u

-- | Completion mode indicator input.
data DoneAction t
  = DoNothing
  | PauseSynth
  | RemoveSynth
  | RemoveGroup
  | DoneAction t
  deriving (Eq, Show)

-- | Resolve 'DoneAction'.
from_done_action :: Num t => DoneAction t -> t
from_done_action e =
    case e of
      DoNothing -> 0
      PauseSynth -> 1
      RemoveSynth -> 2
      RemoveGroup -> 14
      DoneAction x -> x

-- | Warp interpolation indicator input.
data Warp t =
    Linear
  | Exponential
  | Warp t
  deriving (Eq, Show)

-- | Resolve 'Warp'.
from_warp :: Num t => Warp t -> t
from_warp e =
    case e of
      Linear -> 0
      Exponential -> 1
      Warp u -> u

-- | Unification of integer and 'UGen' buffer identifiers.
data Buffer t =
    Buffer_Id Int
  | Buffer t
  deriving (Eq, Show)
