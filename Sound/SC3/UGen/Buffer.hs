module Sound.SC3.UGen.Buffer where

import Sound.SC3.UGen.Rate (Rate(AR))
import Sound.SC3.UGen.UGen (UGen, Name, mkFilter, mkFilterMCE, mkOsc)
import Sound.SC3.UGen.Enum (Loop, fromLoop, Interpolation(..), fromInterpolation)

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

mkBufAllpass :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
mkBufAllpass c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0

-- | Allpass filter (cubic interpolation).
bufAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassC = mkBufAllpass "BufAllpassC"

-- | Allpass filter (linear interpolation).
bufAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassL = mkBufAllpass "BufAllpassL"

-- | Allpass filter (no interpolation).
bufAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassN = mkBufAllpass "BufAllpassN"

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
bufRd :: Int -> Rate -> UGen -> UGen -> Loop -> Interpolation -> UGen
bufRd n r buf phs lp intp = mkOsc r "BufRd" [buf,phs,fromLoop lp,fromInterpolation intp] n 0

-- | Buffer reader (no interpolation).
bufRdN :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdN n r b p l = bufRd n r b p l NoInterpolation

-- | Buffer reader (linear interpolation).
bufRdL :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdL n r b p l = bufRd n r b p l LinearInterpolation

-- | Buffer reader (cubic interpolation).
bufRdC :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdC n r b p l = bufRd n r b p l CubicInterpolation

-- | Buffer writer.
bufWr :: UGen -> UGen -> Loop -> UGen -> UGen
bufWr buf phs lp i = mkFilterMCE "BufWr" [buf,phs,fromLoop lp] i 0 0

-- | Index into table with signal.
index :: UGen -> UGen -> UGen
index b i = mkFilter "Index" [b, i] 1 0

-- | Buffer playback.
playBuf :: Int -> UGen -> UGen -> UGen -> UGen -> Loop -> UGen
playBuf n b r' t s l = mkOsc AR "PlayBuf" [b,r',t,s,fromLoop l] n 0

-- | Triggered buffer shuffler (grain generator).
tGrains :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains n t b r c d p a i = mkFilter "TGrains" [t,b,r,c,d,p,a,i] n 0

-- Local Variables:
-- truncate-lines:t
-- End:
