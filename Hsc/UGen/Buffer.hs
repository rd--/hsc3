module Hsc.UGen.Buffer where

import Hsc.Construct (mkFilter, mkFilterMCE, mkOsc)
import Hsc.Rate (Rate)
import Hsc.UGen (UGen, Name)

bufAllPass' c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0
bufAllPassC = bufAllPass' "BufAllPassC"
bufAllPassL = bufAllPass' "BufAllPassL"
bufAllPassN = bufAllPass' "BufAllPassN"

bufComb' c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0
bufCombC = bufComb' "BufCombC"
bufCombL = bufComb' "BufCombL"
bufCombN = bufComb' "BufCombN"

bufDelay' c buf i dly = mkFilter c [buf,i,dly] 1 0
bufDelayC = bufDelay' "BufDelayC"
bufDelayL = bufDelay' "BufDelayL"
bufDelayN = bufDelay' "BufDelayN"

bufChannels   r buf = mkOsc r "BufChannels"   [buf] 1 0
bufDur        r buf = mkOsc r "BufDur"        [buf] 1 0
bufFrames     r buf = mkOsc r "BufFrames"     [buf] 1 0
bufRateScale  r buf = mkOsc r "BufRateScale"  [buf] 1 0
bufSampleRate r buf = mkOsc r "BufSampleRate" [buf] 1 0
bufSamples    r buf = mkOsc r "BufSamples"    [buf] 1 0

bufRd n r buf phs lp intp = mkOsc r "BufRd" [buf,phs,lp,intp] n 0
bufWr buf phs lp i = mkFilterMCE "BufWr" [buf,phs,lp] i 0 0

playBuf n r b r' t s l = mkOsc r "PlayBuf" [b,r',t,s,l] n 0

tGrains n trg buf rate cntr dur pan amp interp = mkFilter "TGrains" i n 0
    where i = [trg,buf,rate,cntr,dur,pan,amp,interp]


bufAllPassC :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllPassL :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllPassN :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllPass' :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
bufChannels :: Rate -> UGen -> UGen
bufCombC :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombL :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombN :: UGen -> UGen -> UGen -> UGen -> UGen
bufComb' :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
bufDelayC :: UGen -> UGen -> UGen -> UGen
bufDelayL :: UGen -> UGen -> UGen -> UGen
bufDelayN :: UGen -> UGen -> UGen -> UGen
bufDelay' :: Name -> UGen -> UGen -> UGen -> UGen
bufDur :: Rate -> UGen -> UGen
bufFrames :: Rate -> UGen -> UGen
bufRateScale :: Rate -> UGen -> UGen
bufRd :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen
bufSampleRate :: Rate -> UGen -> UGen
bufSamples :: Rate -> UGen -> UGen
bufWr :: UGen -> UGen -> UGen -> UGen -> UGen
playBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
