module Sound.SC3.UGen.Noise where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.UGen (UGen, UId, mkOsc, mkOsc', mkFilter', uniquify, zeroUId)

brownNoise', clipNoise', grayNoise', pinkNoise', whiteNoise' :: UId -> Rate -> UGen
brownNoise' uid r = mkOsc' uid r "BrownNoise" [] 1 0
clipNoise'  uid r = mkOsc' uid r "ClipNoise"  [] 1 0
grayNoise'  uid r = mkOsc' uid r "GrayNoise"  [] 1 0
pinkNoise'  uid r = mkOsc' uid r "PinkNoise"  [] 1 0
whiteNoise' uid r = mkOsc' uid r "WhiteNoise" [] 1 0

lfClipNoise', lfNoise0', lfNoise1', lfNoise2' :: UId -> Rate -> UGen -> UGen
lfClipNoise'  uid r freq = mkOsc' uid r "LFClipNoise"  [freq] 1 0
lfNoise0'     uid r freq = mkOsc' uid r "LFNoise0"     [freq] 1 0
lfNoise1'     uid r freq = mkOsc' uid r "LFNoise1"     [freq] 1 0
lfNoise2'     uid r freq = mkOsc' uid r "LFNoise2"     [freq] 1 0

lfdClipNoise', lfdNoise0', lfdNoise1', lfdNoise2' :: UId -> Rate -> UGen -> UGen
lfdClipNoise' uid r freq = mkOsc' uid r "LFDClipNoise" [freq] 1 0
lfdNoise0'    uid r freq = mkOsc' uid r "LFDNoise0"    [freq] 1 0
lfdNoise1'    uid r freq = mkOsc' uid r "LFDNoise1"    [freq] 1 0
lfdNoise2'    uid r freq = mkOsc' uid r "LFDNoise2"    [freq] 1 0

dust', dust2' :: UId -> Rate -> UGen -> UGen
dust'  uid r density = mkOsc' uid r "Dust"  [density] 1 0
dust2' uid r density = mkOsc' uid r "Dust2" [density] 1 0

tExpRand', tiRand', tRand' :: UId -> UGen -> UGen -> UGen -> UGen
tExpRand' uid lo hi trig = mkFilter' uid "TExpRand" [lo,hi,trig] 1 0
tiRand'   uid lo hi trig = mkFilter' uid "TIRand"   [lo,hi,trig] 1 0
tRand'    uid lo hi trig = mkFilter' uid "TRand"    [lo,hi,trig] 1 0

coinGate' :: UId -> UGen -> UGen -> UGen
coinGate' uid prob i = mkFilter' uid "CoinGate" [prob,i] 1 0

iRand', rand', expRand' :: UId -> UGen -> UGen -> UGen
nRand', linRand' :: UId -> UGen -> UGen -> UGen -> UGen
iRand'   uid lo hi   = mkOsc' uid IR "IRand"   [lo,hi]   1 0
nRand'   uid lo hi n = mkOsc' uid IR "NRand"   [lo,hi,n] 1 0
rand'    uid lo hi   = mkOsc' uid IR "Rand"    [lo,hi]   1 0
expRand' uid lo hi   = mkOsc' uid IR "ExpRand" [lo,hi]   1 0
linRand' uid lo hi m = mkOsc' uid IR "LinRand" [lo,hi,m] 1 0

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

tExpRand, tiRand, tRand :: UGen -> UGen -> UGen -> IO UGen
tExpRand lo hi trig = uniquify (tExpRand' zeroUId lo hi trig)
tiRand   lo hi trig = uniquify (tiRand'   zeroUId lo hi trig)
tRand    lo hi trig = uniquify (tRand'    zeroUId lo hi trig)

coinGate :: UGen -> UGen -> IO UGen
coinGate prob i = uniquify (coinGate' zeroUId prob i)

iRand, rand, expRand :: UGen -> UGen -> IO UGen
nRand, linRand :: UGen -> UGen -> UGen -> IO UGen
iRand   lo hi   = uniquify (iRand'   zeroUId lo hi)
nRand   lo hi n = uniquify (nRand'   zeroUId lo hi n)
rand    lo hi   = uniquify (rand'    zeroUId lo hi)
expRand lo hi   = uniquify (expRand' zeroUId lo hi)
linRand lo hi m = uniquify (linRand' zeroUId lo hi m)

hasher :: Rate -> UGen -> UGen
hasher r i            = mkOsc r "Hasher"       [i]      1 0

mantissaMask :: Rate -> UGen -> UGen -> UGen
mantissaMask r i bits = mkOsc r "MantissaMask" [i,bits] 1 0
