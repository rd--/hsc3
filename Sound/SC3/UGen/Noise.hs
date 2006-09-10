module Sound.SC3.UGen.Noise where

import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UGen (UGen, UId, mkOsc, mkOsc', uniquify, zeroUId)

brownNoise', clipNoise', grayNoise', pinkNoise', whiteNoise' :: UId -> Rate -> UGen
brownNoise' uid r = mkOsc' r "BrownNoise" [] 1 0 uid
clipNoise'  uid r = mkOsc' r "ClipNoise"  [] 1 0 uid
grayNoise'  uid r = mkOsc' r "GrayNoise"  [] 1 0 uid
pinkNoise'  uid r = mkOsc' r "PinkNoise"  [] 1 0 uid
whiteNoise' uid r = mkOsc' r "WhiteNoise" [] 1 0 uid

lfClipNoise', lfNoise0', lfNoise1', lfNoise2' :: UId -> Rate -> UGen -> UGen
lfClipNoise'  uid r freq = mkOsc' r "LFClipNoise"  [freq] 1 0 uid
lfNoise0'     uid r freq = mkOsc' r "LFNoise0"     [freq] 1 0 uid
lfNoise1'     uid r freq = mkOsc' r "LFNoise1"     [freq] 1 0 uid
lfNoise2'     uid r freq = mkOsc' r "LFNoise2"     [freq] 1 0 uid

lfdClipNoise', lfdNoise0', lfdNoise1', lfdNoise2' :: UId -> Rate -> UGen -> UGen
lfdClipNoise' uid r freq = mkOsc' r "LFDClipNoise" [freq] 1 0 uid
lfdNoise0'    uid r freq = mkOsc' r "LFDNoise0"    [freq] 1 0 uid
lfdNoise1'    uid r freq = mkOsc' r "LFDNoise1"    [freq] 1 0 uid
lfdNoise2'    uid r freq = mkOsc' r "LFDNoise2"    [freq] 1 0 uid

dust', dust2' :: UId -> Rate -> UGen -> UGen
dust'  uid r density = mkOsc' r "Dust"  [density] 1 0 uid
dust2' uid r density = mkOsc' r "Dust2" [density] 1 0 uid

tExpRand', tiRand', tRand' :: UId -> Rate -> UGen -> UGen -> UGen -> UGen
tExpRand' uid r lo hi trig = mkOsc' r "TExpRand" [lo,hi,trig] 1 0 uid
tiRand'   uid r lo hi trig = mkOsc' r "TIRand"   [lo,hi,trig] 1 0 uid
tRand'    uid r lo hi trig = mkOsc' r "TRand"    [lo,hi,trig] 1 0 uid

coinGate' :: UId -> Rate -> UGen -> UGen -> UGen
coinGate' uid r prob i = mkOsc' r "CoinGate" [prob,i] 1 0 uid

iRand', rand', expRand' :: UId -> Rate -> UGen -> UGen -> UGen
nRand', linRand' :: UId -> Rate -> UGen -> UGen -> UGen -> UGen
iRand'   uid r lo hi   = mkOsc' r "IRand"   [lo,hi]   1 0 uid
nRand'   uid r lo hi n = mkOsc' r "NRand"   [lo,hi,n] 1 0 uid
rand'    uid r lo hi   = mkOsc' r "Rand"    [lo,hi]   1 0 uid
expRand' uid r lo hi   = mkOsc' r "ExpRand" [lo,hi]   1 0 uid
linRand' uid r lo hi m = mkOsc' r "LinRand" [lo,hi,m] 1 0 uid

brownNoise, clipNoise, grayNoise, pinkNoise, whiteNoise :: Rate -> IO UGen
brownNoise r = uniquify (brownNoise' zeroUId r)
clipNoise  r = uniquify (clipNoise'  zeroUId r)
grayNoise  r = uniquify (grayNoise'  zeroUId r)
pinkNoise  r = uniquify (pinkNoise'  zeroUId r)
whiteNoise r = uniquify (whiteNoise' zeroUId r)

lfClipNoise, lfNoise0, lfNoise1, lfNoise2 :: Rate -> UGen -> IO UGen
lfClipNoise r freq = uniquify (lfClipNoise'  zeroUId r freq)
lfNoise0    r freq = uniquify (lfNoise0'     zeroUId r freq)
lfNoise1    r freq = uniquify (lfNoise1'     zeroUId r freq)
lfNoise2    r freq = uniquify (lfNoise2'     zeroUId r freq)

dust, dust2 :: Rate -> UGen -> IO UGen
dust  r density = uniquify (dust'   zeroUId r density)
dust2 r density = uniquify (dust2'  zeroUId r density)

tExpRand, tiRand, tRand :: Rate -> UGen -> UGen -> UGen -> IO UGen
tExpRand r lo hi trig = uniquify (tExpRand' zeroUId r lo hi trig)
tiRand   r lo hi trig = uniquify (tiRand'   zeroUId r lo hi trig)
tRand    r lo hi trig = uniquify (tRand'    zeroUId r lo hi trig)

coinGate :: Rate -> UGen -> UGen -> IO UGen
coinGate r prob i = uniquify (coinGate' zeroUId r prob i)

iRand, rand, expRand :: Rate -> UGen -> UGen -> IO UGen
nRand, linRand :: Rate -> UGen -> UGen -> UGen -> IO UGen
iRand   r lo hi   = uniquify (iRand'   zeroUId r lo hi)
nRand   r lo hi n = uniquify (nRand'   zeroUId r lo hi n)
rand    r lo hi   = uniquify (rand'    zeroUId r lo hi)
expRand r lo hi   = uniquify (expRand' zeroUId r lo hi)
linRand r lo hi m = uniquify (linRand' zeroUId r lo hi m)

hasher :: Rate -> UGen -> UGen
hasher r i            = mkOsc r "Hasher"       [i]      1 0

mantissaMask :: Rate -> UGen -> UGen -> UGen
mantissaMask r i bits = mkOsc r "MantissaMask" [i,bits] 1 0
