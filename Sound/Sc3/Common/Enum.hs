-- | Data types for enumerated and non signal unit generator inputs.
module Sound.Sc3.Common.Enum where

-- * Loop

-- | Loop indicator input.
data Loop t
  = -- | 0
    NoLoop
  | -- | 1
    Loop
  | WithLoop t
  deriving (Eq, Show)

-- | Apply /f/ at 'WithLoop'.
loop_map :: (t -> u) -> Loop t -> Loop u
loop_map f lp =
  case lp of
    NoLoop -> NoLoop
    Loop -> Loop
    WithLoop t -> WithLoop (f t)

-- | fmap is 'loop_map'
instance Functor Loop where
  fmap = loop_map

-- | Resolve 'Loop'.
from_loop :: Num t => Loop t -> t
from_loop e =
  case e of
    NoLoop -> 0
    Loop -> 1
    WithLoop u -> u

-- * Interpolation

-- | Interpolation indicator input.
data Interpolation t
  = NoInterpolation
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

-- * DoneAction

-- | Completion mode indicator input.
data DoneAction t
  = DoNothing
  | PauseSynth
  | RemoveSynth
  | RemoveGroup
  | WithDoneAction t
  deriving (Eq, Show)

-- | Apply /f/ at 'WithDoneAction'.
done_action_map :: (t -> u) -> DoneAction t -> DoneAction u
done_action_map f e =
  case e of
    DoNothing -> DoNothing
    PauseSynth -> PauseSynth
    RemoveSynth -> RemoveSynth
    RemoveGroup -> RemoveGroup
    WithDoneAction x -> WithDoneAction (f x)

-- | fmap is 'done_action_map'
instance Functor DoneAction where
  fmap = done_action_map

-- | Resolve 'DoneAction'.
from_done_action :: Num t => DoneAction t -> t
from_done_action e =
  case e of
    DoNothing -> 0
    PauseSynth -> 1
    RemoveSynth -> 2
    RemoveGroup -> 14
    WithDoneAction x -> x

-- * Warp

-- | Warp interpolation indicator input.
data Warp t
  = Linear
  | Exponential
  | WithWarp t
  deriving (Eq, Show)

{- | Resolve 'Warp'.

>>> map from_warp [Linear,Exponential]
[0,1]
-}
from_warp :: Num t => Warp t -> t
from_warp e =
  case e of
    Linear -> 0
    Exponential -> 1
    WithWarp u -> u

-- | Apply /f/ at 'WithWarp'
warp_map :: (t -> u) -> Warp t -> Warp u
warp_map f e =
  case e of
    Linear -> Linear
    Exponential -> Exponential
    WithWarp u -> WithWarp (f u)

-- | fmap = 'warp_map'
instance Functor Warp where
  fmap = warp_map

-- * Buffer

-- | Unification of integer and 'Ugen' buffer identifiers.
data Buffer t
  = Buffer_Id Int
  | Buffer t
  deriving (Eq, Show)
