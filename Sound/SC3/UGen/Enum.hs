-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.UGen.Enum where

import Sound.SC3.UGen.Envelope.Interpolate
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
                | DoneAction UGen
                  deriving (Eq, Show)

-- | Resolve 'DoneAction'.
from_done_action :: DoneAction -> UGen
from_done_action e =
    case e of
      DoNothing -> 0
      PauseSynth -> 1
      RemoveSynth -> 2
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
                      | EnvCos -- ^ Note: not implemented at SC3
                      | EnvNum a
                      | EnvSqr
                      | EnvCub
                        deriving (Eq, Show)

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
      EnvCos -> 4
      EnvNum _ -> 5
      EnvSqr -> 6
      EnvCub -> 7

-- | The /value/ of 'EnvCurve' is non-zero for 'EnvNum'.
--
-- > map env_curve_value [EnvCos,EnvNum 2] == [0,2]
env_curve_value :: Num a => Envelope_Curve a -> a
env_curve_value e =
    case e of
      EnvNum u -> u
      _ -> 0

env_curve_interpolation_f :: (Ord t, Floating t) =>
                             Envelope_Curve t -> Interpolation_F t
env_curve_interpolation_f c =
    case c of
      EnvStep -> step
      EnvLin -> linear
      EnvExp -> exponential
      EnvSin -> sine
      EnvCos -> error "env_curve_interpolation_f:EnvCos"
      EnvNum n -> curve n
      EnvSqr -> squared
      EnvCub -> cubed

-- | Enumeration of flags for '/b_gen' command.
data B_Gen = Normalise | Wavetable | Clear
             deriving (Eq,Enum,Bounded,Show)

-- | 'B_Gen' to bit number.
--
-- > map b_gen_bit [minBound .. maxBound]
b_gen_bit :: B_Gen -> Int
b_gen_bit = fromEnum

-- | Set of 'B_Gen' to flag.
--
-- > b_gen_flag [minBound .. maxBound] == 7
b_gen_flag :: [B_Gen] -> Int
b_gen_flag = sum . map ((2 ^) . b_gen_bit)
