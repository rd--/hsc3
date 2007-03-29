module Sound.SC3.UGen.Enum where

import Sound.SC3.UGen.UGen (UGen(..))

data Loop = Loop
          | NoLoop
          | WithLoop UGen
            deriving (Eq, Show)

fromLoop :: Loop -> UGen
fromLoop NoLoop       = Constant 0
fromLoop Loop         = Constant 1
fromLoop (WithLoop u) = u

data Interpolation = NoInterpolation
                   | LinearInterpolation
                   | CubicInterpolation
                   | Interpolation UGen
                     deriving (Eq, Show)

fromInterpolation :: Interpolation -> UGen
fromInterpolation NoInterpolation     = Constant 1
fromInterpolation LinearInterpolation = Constant 2
fromInterpolation CubicInterpolation  = Constant 4
fromInterpolation (Interpolation u)   = u

data DoneAction = DoNothing
                | PauseSynth
                | RemoveSynth
                | DoneAction UGen
                  deriving (Eq, Show)

fromDoneAction :: DoneAction -> UGen
fromDoneAction DoNothing      = Constant 0
fromDoneAction PauseSynth     = Constant 1
fromDoneAction RemoveSynth    = Constant 2
fromDoneAction (DoneAction u) = u

data Warp = Linear
          | Exponential
          | Warp UGen
            deriving (Eq, Show)

fromWarp :: Warp -> UGen
fromWarp Linear      = Constant 0
fromWarp Exponential = Constant 1
fromWarp (Warp u)    = u

