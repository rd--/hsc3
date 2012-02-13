-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.UGen.Enum where

import Sound.SC3.UGen.UGen

-- | Loop indicator input.
data Loop = Loop
          | NoLoop
          | WithLoop UGen
            deriving (Eq, Show)

-- | Interpolation indicator input.
data Interpolation = NoInterpolation
                   | LinearInterpolation
                   | CubicInterpolation
                   | Interpolation UGen
                     deriving (Eq, Show)

-- | Completion mode indicator input.
data DoneAction = DoNothing
                | PauseSynth
                | RemoveSynth
                | DoneAction UGen
                  deriving (Eq, Show)

-- | Warp interpolation indicator input.
data Warp = Linear
          | Exponential
          | Warp UGen
            deriving (Eq, Show)

-- | Envelope curve indicator input.
data Envelope_Curve a = EnvStep
                      | EnvLin
                      | EnvExp
                      | EnvSin
                      | EnvCos
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
