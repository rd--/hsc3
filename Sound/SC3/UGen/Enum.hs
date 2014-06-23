-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.UGen.Enum where

import Sound.SC3.Internal
import qualified Sound.SC3.UGen.Envelope.Interpolate as I
import Sound.SC3.UGen.Type

-- | Loop indicator input.
data Loop = Loop
          | NoLoop
          | WithLoop UGen
            deriving (Eq, Show)

-- | Resolve 'Loop'.
from_loop :: Loop -> UGen
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

-- | Envelope curve indicator input.
data Envelope_Curve a = EnvStep
                      | EnvLin
                      | EnvExp
                      | EnvSin
                      | EnvWelch -- ^ Note: not implemented at SC3
                      | EnvNum a
                      | EnvSqr
                      | EnvCub
                      | EnvHold
                        deriving (Eq, Show)

-- | Envelope curve pair.
type Envelope_Curve2 a = T2 (Envelope_Curve a)

-- | Envelope curve triple.
type Envelope_Curve3 a = T3 (Envelope_Curve a)

-- | Envelope curve triple.
type Envelope_Curve4 a = T4 (Envelope_Curve a)

-- | Type specialised ('UGen') envelope curve.
type EnvCurve = Envelope_Curve UGen

-- | Convert 'Envelope_Curve' to shape value.
--
-- > map env_curve_shape [EnvSin,EnvSqr] == [3,6]
env_curve_shape :: Num a => Envelope_Curve a -> a
env_curve_shape e =
    case e of
      EnvStep -> 0
      EnvLin -> 1
      EnvExp -> 2
      EnvSin -> 3
      EnvWelch -> 4
      EnvNum _ -> 5
      EnvSqr -> 6
      EnvCub -> 7
      EnvHold -> 8

-- | The /value/ of 'EnvCurve' is non-zero for 'EnvNum'.
--
-- > map env_curve_value [EnvWelch,EnvNum 2] == [0,2]
env_curve_value :: Num a => Envelope_Curve a -> a
env_curve_value e =
    case e of
      EnvNum u -> u
      _ -> 0

-- | 'Interpolation_F' of 'Envelope_Curve'.
env_curve_interpolation_f :: (Ord t, Floating t) =>
                             Envelope_Curve t -> I.Interpolation_F t
env_curve_interpolation_f c =
    case c of
      EnvStep -> I.step
      EnvLin -> I.linear
      EnvExp -> I.exponential
      EnvSin -> I.sine
      EnvWelch -> I.welch
      EnvNum n -> I.curve n
      EnvSqr -> I.squared
      EnvCub -> I.cubed
      EnvHold -> undefined

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
