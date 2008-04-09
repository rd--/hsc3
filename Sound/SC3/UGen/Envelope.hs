module Sound.SC3.UGen.Envelope where

import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Utilities

-- | Segment based envelope generator.
envGen :: Rate -> UGen -> UGen -> UGen -> UGen -> DoneAction -> [UGen] -> UGen
envGen r gate lvl bias scale act pts = mkOsc r "EnvGen" i 1
 where i = [gate, lvl, bias, scale, from_done_action act] ++ pts

-- | Line generator.
line :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
line r start end dur act = mkOsc r "Line" [start, end, dur, from_done_action act] 1

-- | Exponential line generator.
xLine :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
xLine r start end dur act = mkOsc r "XLine" [start, end, dur, from_done_action act] 1

-- | Free node on trigger.
freeSelf :: UGen -> UGen
freeSelf i = mkFilter "FreeSelf" [i] 0

-- | Free node on done action at source.
freeSelfWhenDone :: UGen -> UGen
freeSelfWhenDone i = mkFilter "FreeSelfWhenDone" [i] 1

-- | Pause specified node on trigger.
pause :: UGen -> UGen -> UGen
pause t n = mkFilter "Pause" [t, n] 1

-- | Pause node on trigger.
pauseSelf :: UGen -> UGen
pauseSelf i = mkFilter "PauseSelf" [i] 0

-- | Pause node on done action at source.
pauseSelfWhenDone :: UGen -> UGen
pauseSelfWhenDone i = mkFilter "PauseSelfWhenDone" [i] 0

-- | One while the source is marked done, else zero.
done :: UGen -> UGen
done i = mkFilter "Done" [i] 1

-- | Raise specified done action when input goes silent.
detectSilence ::  UGen -> UGen -> UGen -> DoneAction -> UGen
detectSilence i a t act = mkFilter "DetectSilence" [i, a, t, from_done_action act] 0

-- | When triggered free specified node.
free :: UGen -> UGen -> UGen
free i n = mkFilter "Free" [i, n] 1

-- | Linear envelope generator.
linen :: UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
linen g at sl rt da = mkFilter "Linen" [g, at, sl, rt, from_done_action da] 1
