module Hsc.UGens.Noise where

import Hsc.Construct (mkOsc, mkOsc', uniquify, zeroUId)

brownNoise' id r = mkOsc' r "BrownNoise" [] 1 0 id
clipNoise'  id r = mkOsc' r "ClipNoise"  [] 1 0 id
grayNoise'  id r = mkOsc' r "GrayNoise"  [] 1 0 id
pinkNoise'  id r = mkOsc' r "PinkNoise"  [] 1 0 id
whiteNoise' id r = mkOsc' r "WhiteNoise" [] 1 0 id

lfClipNoise'  id r freq = mkOsc' r "LFClipNoise"  [freq] 1 0 id
lfNoise0'     id r freq = mkOsc' r "LFNoise0"     [freq] 1 0 id
lfNoise1'     id r freq = mkOsc' r "LFNoise1"     [freq] 1 0 id
lfNoise2'     id r freq = mkOsc' r "LFNoise2"     [freq] 1 0 id

lfdClipNoise' id r freq = mkOsc' r "LFDClipNoise" [freq] 1 0 id
lfdNoise0'    id r freq = mkOsc' r "LFDNoise0"    [freq] 1 0 id
lfdNoise1'    id r freq = mkOsc' r "LFDNoise1"    [freq] 1 0 id
lfdNoise2'    id r freq = mkOsc' r "LFDNoise2"    [freq] 1 0 id

dust'  id r density = mkOsc' r "Dust"  [density] 1 0 id
dust2' id r density = mkOsc' r "Dust2" [density] 1 0 id

tExpRand' id r lo hi trig = mkOsc' r "TExpRand" [lo,hi,trig] 1 0 id
tiRand'   id r lo hi trig = mkOsc' r "TIRand"   [lo,hi,trig] 1 0 id
tRand'    id r lo hi trig = mkOsc' r "TRand"    [lo,hi,trig] 1 0 id

coinGate' id r prob i = mkOsc' r "CoinGate" [prob,i] 1 0 id

iRand'   id r lo hi   = mkOsc' r "IRand"   [lo,hi]   1 0 id
nRand'   id r lo hi n = mkOsc' r "NRand"   [lo,hi,n] 1 0 id
rand'    id r lo hi   = mkOsc' r "Rand"    [lo,hi]   1 0 id
expRand' id r lo hi   = mkOsc' r "ExpRand" [lo,hi]   1 0 id
linRand' id r lo hi m = mkOsc' r "LinRand" [lo,hi,m] 1 0 id

brownNoise r = uniquify (brownNoise' zeroUId r)
clipNoise r  = uniquify (clipNoise'  zeroUId r)
grayNoise r  = uniquify (grayNoise'  zeroUId r)
pinkNoise r  = uniquify (pinkNoise'  zeroUId r)
whiteNoise r = uniquify (whiteNoise' zeroUId r)

lfClipNoise r freq = uniquify (lfClipNoise'  zeroUId r freq)
lfNoise0    r freq = uniquify (lfNoise0'     zeroUId r freq)
lfNoise1    r freq = uniquify (lfNoise1'     zeroUId r freq)
lfNoise2    r freq = uniquify (lfNoise2'     zeroUId r freq)

dust  r density = uniquify (dust'   zeroUId r density)
dust2 r density = uniquify (dust2'  zeroUId r density)

tExpRand r lo hi trig = uniquify (tExpRand' zeroUId r lo hi trig)
tiRand   r lo hi trig = uniquify (tiRand'   zeroUId r lo hi trig)
tRand    r lo hi trig = uniquify (tRand'    zeroUId r lo hi trig)

coinGate r prob i = uniquify (coinGate' zeroUId r prob i)

iRand   r lo hi   = uniquify (iRand'   zeroUId r lo hi)
nRand   r lo hi n = uniquify (nRand'   zeroUId r lo hi n)
rand    r lo hi   = uniquify (rand'    zeroUId r lo hi)
expRand r lo hi   = uniquify (expRand' zeroUId r lo hi)
linRand r lo hi m = uniquify (linRand' zeroUId r lo hi m)

hasher r i            = mkOsc r "Hasher"       [i]      1 0
mantissaMask r i bits = mkOsc r "MantissaMask" [i,bits] 1 0
