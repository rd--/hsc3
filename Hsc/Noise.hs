module Hsc.Noise where

import Hsc.UGen

brownnoise id r = UGen r "BrownNoise" [] [r] 0 id
clipnoise  id r = UGen r "ClipNoise"  [] [r] 0 id
graynoise  id r = UGen r "GrayNoise"  [] [r] 0 id
pinknoise  id r = UGen r "PinkNoise"  [] [r] 0 id
whitenoise id r = UGen r "WhiteNoise" [] [r] 0 id

lfclipnoise id r freq = UGen r "LFClipNoise" [freq] [r] 0 id
lfnoise0 id r freq    = UGen r "LFNoise0"    [freq] [r] 0 id
lfnoise1 id r freq    = UGen r "LFNoise1"    [freq] [r] 0 id
lfnoise2 id r freq    = UGen r "LFNoise2"    [freq] [r] 0 id

dust  id r density = UGen r "Dust"  [density] [r] 0 id
dust2 id r density = UGen r "Dust2" [density] [r] 0 id

linrand id r lo hi m = UGen r "LinRand" [lo,hi,m] [r] 0 id
