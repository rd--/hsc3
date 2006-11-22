module Sound.SC3.UGen.IO where

import Sound.SC3.UGen.Rate (Rate(AR, KR))
import Sound.SC3.UGen.UGen (UGen(Constant),  mkOsc,  mkOscMCE, mkFilterMCE, hasOutputs)

data Warp = Linear
          | Exponential
          | Warp UGen
            deriving (Eq, Show)

fromWarp :: Warp -> UGen
fromWarp Linear      = Constant 0
fromWarp Exponential = Constant 1
fromWarp (Warp u)    = u

in' :: Rate -> UGen -> Int -> UGen
in' r bus nc = mkOsc r "In" [bus] nc 0

localIn :: Rate -> Int -> UGen
localIn r nc = mkOsc r "In" [] nc 0

lagIn :: UGen -> Int -> UGen -> UGen
lagIn bus nc lag = mkOsc KR "LagIn" [bus, lag] nc 0

inFeedback :: UGen -> Int -> UGen
inFeedback bus nc = mkOsc AR "InFeedback" [bus] nc 0

inTrig :: UGen -> Int -> UGen
inTrig bus nc = mkOsc KR "InTrig" [bus] nc 0

out :: UGen -> UGen -> UGen
out bus i = mkFilterMCE "Out" [bus] i 0 0

replaceOut :: UGen -> UGen -> UGen
replaceOut bus i = mkFilterMCE "ReplaceOut" [bus] i 0 0

offsetOut :: UGen -> UGen -> UGen
offsetOut bus i = mkOscMCE AR "OffsetOut" [bus] i 0 0

localOut :: UGen -> UGen
localOut i = mkFilterMCE "LocalOut" [] i 0 0

xOut :: UGen -> UGen -> UGen -> UGen
xOut bus xfade i = mkFilterMCE "XOut" [bus, xfade] i 0 0

sharedOut :: UGen -> UGen -> UGen
sharedOut bus i = mkOscMCE KR "SharedOut" [bus] i 0 0

sharedIn :: UGen -> Int -> UGen
sharedIn bus nc = mkOsc KR "SharedIn" [bus] nc 0

keyState :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
keyState r key minVal maxVal lag = mkOsc r "KeyState" [key, minVal, maxVal, lag] 1 0

mouseButton :: Rate -> UGen -> UGen -> UGen -> UGen
mouseButton r minVal maxVal lag = mkOsc r "MouseButton" [minVal, maxVal, lag] 1 0

mouseX :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseX r minVal maxVal warp lag = mkOsc r "MouseX" [minVal, maxVal, fromWarp warp, lag] 1 0

mouseY :: Rate -> UGen -> UGen -> Warp -> UGen -> UGen
mouseY r minVal maxVal warp lag = mkOsc r "MouseY" [minVal, maxVal, fromWarp warp, lag] 1 0

-- * Utilities

-- | If the UGen has output ports connect it to an 'out' UGen.
addOut :: UGen -> UGen
addOut u = if hasOutputs u then out (Constant 0) u else u

-- Local Variables:
-- truncate-lines:t
-- End:
