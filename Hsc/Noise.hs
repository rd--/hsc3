module Hsc.Noise where

import Hsc.UGen

brownnoise r id = UGen r "BrownNoise" [] [r] 0 id
clipnoise  r id = UGen r "ClipNoise"  [] [r] 0 id
graynoise  r id = UGen r "GrayNoise"  [] [r] 0 id
pinknoise  r id = UGen r "PinkNoise"  [] [r] 0 id
whitenoise r id = UGen r "WhiteNoise" [] [r] 0 id

lfclipnoise r id freq = UGen r "LFClipNoise" [freq] [r] 0 id
lfnoise0 r id freq    = UGen r "LFNoise0"    [freq] [r] 0 id
lfnoise1 r id freq    = UGen r "LFNoise1"    [freq] [r] 0 id
lfnoise2 r id freq    = UGen r "LFNoise2"    [freq] [r] 0 id

dust  r id density = UGen r "Dust"  [density] [r] 0 id
dust2 r id density = UGen r "Dust2" [density] [r] 0 id

linrand id r lo hi m = UGen r "LinRand" [lo,hi,m] [r] 0 id
