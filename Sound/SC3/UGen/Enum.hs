module Sound.SC3.UGen.Enum where

import Sound.SC3.UGen.UGen (UGen)

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
data EnvCurve = EnvStep
              | EnvLin
              | EnvExp
              | EnvSin
              | EnvCos
              | EnvNum UGen
              | EnvSqr
              | EnvCub
              deriving (Eq, Show)
