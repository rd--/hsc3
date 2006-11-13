module Sound.SC3.UGen.Buffer where

import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UGen (UGen, Name, mkFilter, mkFilterMCE, mkOsc)

-- * Buffer query UGens.

-- | Buffer channel count.
bufChannels :: Rate -> UGen -> UGen
bufChannels r buf = mkOsc r "BufChannels" [buf] 1 0

-- | Buffer duration, in seconds.
bufDur :: Rate -> UGen -> UGen
bufDur r buf = mkOsc r "BufDur" [buf] 1 0

-- | Buffer frame count.
bufFrames :: Rate -> UGen -> UGen
bufFrames r buf = mkOsc r "BufFrames" [buf] 1 0

-- | Buffer rate scalar with respect to server sample rate.
bufRateScale :: Rate -> UGen -> UGen
bufRateScale r buf = mkOsc r "BufRateScale" [buf] 1 0

-- | Buffer sample rate.
bufSampleRate :: Rate -> UGen -> UGen
bufSampleRate r buf = mkOsc r "BufSampleRate" [buf] 1 0

-- | Buffer sample count (ie. frame count by channel count).
bufSamples :: Rate -> UGen -> UGen
bufSamples r buf = mkOsc r "BufSamples" [buf] 1 0

-- * Buffer filters and delays.

mkBufAllPass :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
mkBufAllPass c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0

-- | Allpass filter (cubic interpolation).
bufAllPassC :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllPassC = mkBufAllPass "BufAllPassC"

-- | Allpass filter (linear interpolation).
bufAllPassL :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllPassL = mkBufAllPass "BufAllPassL"

-- | Allpass filter (no interpolation).
bufAllPassN :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllPassN = mkBufAllPass "BufAllPassN"

mkBufComb :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
mkBufComb c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0

-- | Comb filter (cubic interpolation).
bufCombC :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombC = mkBufComb "BufCombC"

-- | Comb filter (linear interpolation).
bufCombL :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombL = mkBufComb "BufCombL"

-- | Comb filter (no interpolation).
bufCombN :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombN = mkBufComb "BufCombN"

mkBufDelay :: Name -> UGen -> UGen -> UGen -> UGen
mkBufDelay c buf i dly = mkFilter c [buf,i,dly] 1 0

-- | Delay line (cubic interpolation).
bufDelayC :: UGen -> UGen -> UGen -> UGen
bufDelayC = mkBufDelay "BufDelayC"

-- | Delay line (linear interpolation).
bufDelayL :: UGen -> UGen -> UGen -> UGen
bufDelayL = mkBufDelay "BufDelayL"

-- | Delay line (no interpolation).
bufDelayN :: UGen -> UGen -> UGen -> UGen
bufDelayN = mkBufDelay "BufDelayN"

-- * Buffer I\/O.

-- | Buffer reader.
bufRd :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen
bufRd n r buf phs lp intp = mkOsc r "BufRd" [buf,phs,lp,intp] n 0

-- | Buffer writer.
bufWr buf phs lp i = mkFilterMCE "BufWr" [buf,phs,lp] i 0 0
bufWr :: UGen -> UGen -> UGen -> UGen -> UGen

-- | Buffer playback.
playBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
playBuf n r b r' t s l = mkOsc r "PlayBuf" [b,r',t,s,l] n 0

-- | Triggered buffer shuffler (grain generator).
tGrains :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains n t b r c d p a i = mkFilter "TGrains" [t,b,r,c,d,p,a,i] n 0
