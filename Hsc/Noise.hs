module Hsc.Noise where

import Hsc.UGen

brownnoise id r = UGen r "BrownNoise" [] [r] 0 id
clipnoise  id r = UGen r "ClipNoise"  [] [r] 0 id
graynoise  id r = UGen r "GrayNoise"  [] [r] 0 id
pinknoise  id r = UGen r "PinkNoise"  [] [r] 0 id
whitenoise id r = UGen r "WhiteNoise" [] [r] 0 id

lfclipnoise  id r freq = UGen r "LFClipNoise"  [freq] [r] 0 id
lfnoise0     id r freq = UGen r "LFNoise0"     [freq] [r] 0 id
lfnoise1     id r freq = UGen r "LFNoise1"     [freq] [r] 0 id
lfnoise2     id r freq = UGen r "LFNoise2"     [freq] [r] 0 id

lfdclipnoise id r freq = UGen r "LFDClipNoise" [freq] [r] 0 id
lfdnoise0    id r freq = UGen r "LFDNoise0"    [freq] [r] 0 id
lfdnoise1    id r freq = UGen r "LFDNoise1"    [freq] [r] 0 id
lfdnoise2    id r freq = UGen r "LFDNoise2"    [freq] [r] 0 id

dust  id r density = UGen r "Dust"  [density] [r] 0 id
dust2 id r density = UGen r "Dust2" [density] [r] 0 id

texprand id r lo hi trig = UGen r "TExpRand" [lo,hi,trig] [r] 0 id
tirand   id r lo hi trig = UGen r "TIRand"   [lo,hi,trig] [r] 0 id
trand    id r lo hi trig = UGen r "TRand"    [lo,hi,trig] [r] 0 id

coingate id r prob i = UGen r "CoinGate" [prob,i] [r] 0 id

irand   id r lo hi   = UGen r "IRand"   [lo,hi]   [r] 0 id
nrand   id r lo hi n = UGen r "NRand"   [lo,hi,n] [r] 0 id
rand    id r lo hi   = UGen r "Rand"    [lo,hi]   [r] 0 id
exprand id r lo hi   = UGen r "ExpRand" [lo,hi]   [r] 0 id
linrand id r lo hi m = UGen r "LinRand" [lo,hi,m] [r] 0 id

hasher r i            = UGen r "Hasher"       [i]      [r] 0 r0
mantissamask r i bits = UGen r "MantissaMask" [i,bits] [r] 0 r0
