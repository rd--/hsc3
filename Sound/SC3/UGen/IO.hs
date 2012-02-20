-- | Audio bus, control bus and input device unit generators.
module Sound.SC3.UGen.IO where

import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen

-- | Read signal from an audio or control bus.
in' :: Int -> Rate -> UGen -> UGen
in' nc r bus = mkOsc r "In" [bus] nc

-- | Define and read from buses local to a synthesis node.
localIn :: Int -> Rate -> UGen
localIn nc r = mkOsc r "LocalIn" [] nc

-- | Control rate bus input with lag.
lagIn :: Int -> UGen -> UGen -> UGen
lagIn nc bus lag = mkOsc KR "LagIn" [bus, lag] nc

-- | Read signal from a bus without erasing it.
inFeedback :: Int -> UGen -> UGen
inFeedback nc bus = mkOsc AR "InFeedback" [bus] nc

-- | Generate a trigger anytime a bus is set.
inTrig :: Int -> UGen -> UGen
inTrig nc bus = mkOsc KR "InTrig" [bus] nc

-- | Mix signal to an audio or control bus.
out :: UGen -> UGen -> UGen
out bus i = mkFilterMCE "Out" [bus] i 0

-- | Over-write signal to an audio or control bus.
replaceOut :: UGen -> UGen -> UGen
replaceOut bus i = mkFilterMCE "ReplaceOut" [bus] i 0

-- | Mix signal to an audio bus at precise sample offset.
offsetOut :: UGen -> UGen -> UGen
offsetOut bus i = mkOscMCE AR "OffsetOut" [bus] i 0

-- | Write signal to bus local to a synthesis node, see localIn.
localOut :: UGen -> UGen
localOut i = mkFilterMCE "LocalOut" [] i 0

-- | Crossfade signal to an audio or control bus.
xOut :: UGen -> UGen -> UGen -> UGen
xOut bus xfade i = mkFilterMCE "XOut" [bus, xfade] i 0

-- | Write to a shared control bus.
sharedOut :: UGen -> UGen -> UGen
sharedOut bus i = mkOscMCE KR "SharedOut" [bus] i 0

-- | Read from a shared control bus.
sharedIn :: Int -> UGen -> UGen
sharedIn nc bus = mkOsc KR "SharedIn" [bus] nc

-- | Report the status of a particular key.
keyState :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
keyState r key minVal maxVal lag = mkOscR [KR] r "KeyState" [key, minVal, maxVal, lag] 1

-- | Report the status of the first pointer button.
mouseButton :: Rate -> UGen -> UGen -> UGen -> UGen
mouseButton r ll rl lag = mkOscR [KR] r "MouseButton" [ll, rl, lag] 1

-- | Cursor UGen, X axis.
mouseX :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseX r ll rl w lag = mkOscR [KR] r "MouseX" [ll, rl, from_warp w, lag] 1

-- | Cursor UGen, Y axis.
mouseY :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseY r ll rl w lag = mkOscR [KR] r "MouseY" [ll, rl, from_warp w, lag] 1

-- | Control variant.
trigControl :: Int -> Rate -> UGen
trigControl nc r = mkOsc r "TrigControl" [] nc

-- | Set the synth's random generator ID.
randID :: Rate -> UGen -> UGen
randID r n = mkOsc r "RandID" [n] 1

-- | Set the synth's random generator seed.
randSeed :: Rate -> UGen -> UGen -> UGen
randSeed r tr sd = mkOsc r "RandSeed" [tr,sd] 1

-- Local Variables:
-- truncate-lines:t
-- End:
