module Sound.SC3.UGen.Private where

import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.UGen (UGen(..), mkOsc)
import Sound.SC3.UGen.Rate (Rate(IR))

fromLoop :: Loop -> UGen
fromLoop NoLoop       = Constant 0
fromLoop Loop         = Constant 1
fromLoop (WithLoop u) = u

fromInterpolation :: Interpolation -> UGen
fromInterpolation NoInterpolation     = Constant 1
fromInterpolation LinearInterpolation = Constant 2
fromInterpolation CubicInterpolation  = Constant 4
fromInterpolation (Interpolation u)   = u

fromDoneAction :: DoneAction -> UGen
fromDoneAction DoNothing      = Constant 0
fromDoneAction PauseSynth     = Constant 1
fromDoneAction RemoveSynth    = Constant 2
fromDoneAction (DoneAction u) = u

fromWarp :: Warp -> UGen
fromWarp Linear      = Constant 0
fromWarp Exponential = Constant 1
fromWarp (Warp u)    = u

env_curve :: EnvCurve -> UGen
env_curve EnvStep    = 0.0
env_curve EnvLin     = 1.0
env_curve EnvExp     = 2.0
env_curve EnvSin     = 3.0
env_curve EnvCos     = 4.0
env_curve (EnvNum _) = 5.0
env_curve EnvSqr     = 6.0
env_curve EnvCub     = 7.0

env_value :: EnvCurve -> UGen
env_value (EnvNum u) = u
env_value _          = 0.0

d_dx :: (Num a) => [a] -> [a]
d_dx [] = []
d_dx [_] = []
d_dx [x,y] = [y - x]
d_dx (x:y:r) = y - x : d_dx (y:r)

dbl :: a -> [a]
dbl x = [x,x]

mkInfoUGen :: String -> UGen
mkInfoUGen name = mkOsc IR name [] 1

