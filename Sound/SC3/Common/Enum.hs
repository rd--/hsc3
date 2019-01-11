-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.Common.Enum where

-- | Loop indicator input.
data Loop t =
    Loop
  | NoLoop
  | WithLoop t
  deriving (Eq, Show)

-- | Apply /f/ at 'WithLoop'.
loop_coerce :: (t -> u) -> Loop t -> Loop u
loop_coerce f lp =
  case lp of
    Loop -> Loop
    NoLoop -> NoLoop
    WithLoop t -> WithLoop (f t)

-- | fmap is 'loop_coerce'
instance Functor Loop where
  fmap = loop_coerce

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
  | WithInterpolation t
  deriving (Eq, Show)

-- | Resolve 'Interpolation'.
from_interpolation :: Num t => Interpolation t -> t
from_interpolation e =
    case e of
      NoInterpolation -> 1
      LinearInterpolation -> 2
      CubicInterpolation -> 4
      WithInterpolation u -> u

-- | Completion mode indicator input.
data DoneAction t
  = DoNothing
  | PauseSynth
  | RemoveSynth
  | RemoveGroup
  | WithDoneAction t
  deriving (Eq, Show)

-- | Apply /f/ at 'WithDoneAction'.
done_action_coerce :: (t -> u) -> DoneAction t -> DoneAction u
done_action_coerce f e =
    case e of
      DoNothing -> DoNothing
      PauseSynth -> PauseSynth
      RemoveSynth -> RemoveSynth
      RemoveGroup -> RemoveGroup
      WithDoneAction x -> WithDoneAction (f x)

-- | fmap is 'done_action_coerce'
instance Functor DoneAction where
  fmap = done_action_coerce

-- | Resolve 'DoneAction'.
from_done_action :: Num t => DoneAction t -> t
from_done_action e =
    case e of
      DoNothing -> 0
      PauseSynth -> 1
      RemoveSynth -> 2
      RemoveGroup -> 14
      WithDoneAction x -> x

-- | Warp interpolation indicator input.
data Warp t =
    Linear
  | Exponential
  | WithWarp t
  deriving (Eq, Show)

-- | Resolve 'Warp'.
from_warp :: Num t => Warp t -> t
from_warp e =
    case e of
      Linear -> 0
      Exponential -> 1
      WithWarp u -> u

-- | Apply /f/ at 'WithWarp'
warp_coerce :: (t -> u) -> Warp t -> Warp u
warp_coerce f e =
    case e of
      Linear -> Linear
      Exponential -> Exponential
      WithWarp u -> WithWarp (f u)

-- | fmap = 'warp_coerce'
instance Functor Warp where
  fmap = warp_coerce

-- | Unification of integer and 'UGen' buffer identifiers.
data Buffer t =
    Buffer_Id Int
  | Buffer t
  deriving (Eq, Show)
