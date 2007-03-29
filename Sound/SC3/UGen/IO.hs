module Sound.SC3.UGen.IO where

import Sound.SC3.UGen.Rate (Rate(AR, KR))
import Sound.SC3.UGen.UGen (UGen(Constant),  mkOsc,  mkOscMCE, mkFilterMCE, hasOutputs)
import Sound.SC3.UGen.Enum (Warp, fromWarp)

-- | Read signal from an audio or control bus.
in' :: Int -> Rate -> UGen -> UGen
in' nc r bus = mkOsc r "In" [bus] nc 0

-- | Define and read from buses local to a synthesis node.
localIn :: Int -> Rate -> UGen
localIn nc r = mkOsc r "LocalIn" [] nc 0

lagIn :: Int -> UGen -> UGen -> UGen
lagIn nc bus lag = mkOsc KR "LagIn" [bus, lag] nc 0

-- | Read signal from a bus without erasing it.
inFeedback :: Int -> UGen -> UGen
inFeedback nc bus = mkOsc AR "InFeedback" [bus] nc 0

-- | Generate a trigger anytime a bus is set.
inTrig :: Int -> UGen -> UGen
inTrig nc bus = mkOsc KR "InTrig" [bus] nc 0

-- | Mix signal to an audio or control bus.
out :: UGen -> UGen -> UGen
out bus i = mkFilterMCE "Out" [bus] i 0 0

-- | Over-write signal to an audio or control bus.
replaceOut :: UGen -> UGen -> UGen
replaceOut bus i = mkFilterMCE "ReplaceOut" [bus] i 0 0

-- | Mix signal to an audio bus at precise sample offset.
offsetOut :: UGen -> UGen -> UGen
offsetOut bus i = mkOscMCE AR "OffsetOut" [bus] i 0 0

-- | Write signal to bus local to a synthesis node, see localIn.
localOut :: UGen -> UGen
localOut i = mkFilterMCE "LocalOut" [] i 0 0

-- | Crossfade signal to an audio or control bus.
xOut :: UGen -> UGen -> UGen -> UGen
xOut bus xfade i = mkFilterMCE "XOut" [bus, xfade] i 0 0

sharedOut :: UGen -> UGen -> UGen
sharedOut bus i = mkOscMCE KR "SharedOut" [bus] i 0 0

sharedIn :: Int -> UGen -> UGen
sharedIn nc bus = mkOsc KR "SharedIn" [bus] nc 0

-- | Report the status of a particular key.
keyState :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
keyState r key minVal maxVal lag = mkOsc r "KeyState" [key, minVal, maxVal, lag] 1 0

-- | Report the status of the first pointer button.
mouseButton :: Rate -> UGen -> UGen -> UGen -> UGen
mouseButton r minVal maxVal lag = mkOsc r "MouseButton" [minVal, maxVal, lag] 1 0

-- | Cursor UGen, X axis.
mouseX :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseX r minVal maxVal warp lag = mkOsc r "MouseX" [minVal, maxVal, fromWarp warp, lag] 1 0

-- | Cursor UGen, Y axis.
mouseY :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseY r minVal maxVal warp lag = mkOsc r "MouseY" [minVal, maxVal, fromWarp warp, lag] 1 0

-- * Utilities

-- | If the UGen has output ports connect it to an 'out' UGen.
addOut :: UGen -> UGen
addOut u = if hasOutputs u then out (Constant 0) u else u

-- Local Variables:
-- truncate-lines:t
-- End:
