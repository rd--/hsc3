module Sound.SC3.UGen.Demand where

import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UGen (UGen, mkOsc, mkOscMCE, mkFilterKeyed)
import Sound.SC3.UGen.Enum (DoneAction)
import Sound.SC3.UGen.Private (fromDoneAction)

-- | Demand results from demand rate ugens.
demand :: UGen -> UGen -> UGen -> UGen
demand t r d = mkFilterKeyed "Demand" 0 [t, r, d] 1 0

-- | Demand results from demand rate ugens.
duty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen
duty rate d r act l = mkOsc rate "Duty" [d, r, fromDoneAction act, l] 1 0

-- | Demand results as trigger from demand rate ugens.
tDuty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen
tDuty r d rst act l = mkOscMCE r "TDuty" [d,rst,fromDoneAction act] l 1 0
