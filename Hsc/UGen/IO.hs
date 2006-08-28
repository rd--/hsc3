module Hsc.UGen.IO where

import Hsc.Construct (mkOsc, mkFilterMCE)
import Hsc.Rate (Rate)
import Hsc.UGen (UGen)

in' n r bus = mkOsc r "In" [bus] n 0

keyState r key minVal maxVal lag = mkOsc r "KeyState" [key,minVal,maxVal,lag] 1 0

mouseButton r minVal maxVal lag  = mkOsc r "MouseButton"  [minVal,maxVal,lag] 1 0
mouseX r minVal maxVal warp lag  = mkOsc r "MouseX"  [minVal,maxVal,warp,lag] 1 0
mouseY r minVal maxVal warp lag  = mkOsc r "MouseY"  [minVal,maxVal,warp,lag] 1 0

out b i        = mkFilterMCE "Out"        [b] i 0 0
replaceOut b i = mkFilterMCE "ReplaceOut" [b] i 0 0


in' :: Int -> Rate -> UGen -> UGen
keyState :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
mouseButton :: Rate -> UGen -> UGen -> UGen -> UGen
mouseX :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
mouseY :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
out :: UGen -> UGen -> UGen
replaceOut :: UGen -> UGen -> UGen
