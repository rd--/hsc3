-- | Unit generators to query, read and write audio buffers.
module Sound.SC3.UGen.Buffer where

import Sound.SC3.Identifier
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Utilities

-- * Buffer query unit generators

-- | Buffer channel count.
bufChannels :: Rate -> UGen -> UGen
bufChannels r buf = mkOsc r "BufChannels" [buf] 1

-- | Buffer duration, in seconds.
bufDur :: Rate -> UGen -> UGen
bufDur r buf = mkOsc r "BufDur" [buf] 1

-- | Buffer frame count.
bufFrames :: Rate -> UGen -> UGen
bufFrames r buf = mkOsc r "BufFrames" [buf] 1

-- | Buffer rate scalar with respect to server sample rate.
bufRateScale :: Rate -> UGen -> UGen
bufRateScale r buf = mkOsc r "BufRateScale" [buf] 1

-- | Buffer sample rate.
bufSampleRate :: Rate -> UGen -> UGen
bufSampleRate r buf = mkOsc r "BufSampleRate" [buf] 1

-- | Buffer sample count (ie. frame count by channel count).
bufSamples :: Rate -> UGen -> UGen
bufSamples r buf = mkOsc r "BufSamples" [buf] 1

-- * Buffer filters and delays

-- | Allpass filter (cubic interpolation).
bufAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassC buf i dly dcy = mkFilter "BufAllpassC" [buf, i, dly, dcy] 1

-- | Allpass filter (linear interpolation).
bufAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassL buf i dly dcy = mkFilter "BufAllpassL" [buf, i, dly, dcy] 1

-- | Allpass filter (no interpolation).
bufAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassN buf i dly dcy = mkFilter "BufAllpassN" [buf, i, dly, dcy] 1

-- | Comb filter (cubic interpolation).
bufCombC :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombC buf i dly dcy = mkFilter "BufCombC" [buf, i, dly, dcy] 1

-- | Comb filter (linear interpolation).
bufCombL :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombL buf i dly dcy = mkFilter "BufCombL" [buf, i, dly, dcy] 1

-- | Comb filter (no interpolation).
bufCombN :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombN buf i dly dcy = mkFilter "BufCombN" [buf, i, dly, dcy] 1

-- | Delay line (cubic interpolation).
bufDelayC :: UGen -> UGen -> UGen -> UGen
bufDelayC buf i dly = mkFilter "BufDelayC" [buf, i, dly] 1

-- | Delay line (linear interpolation).
bufDelayL :: UGen -> UGen -> UGen -> UGen
bufDelayL buf i dly = mkFilter "BufDelayL" [buf, i, dly] 1

-- | Delay line (no interpolation).
bufDelayN :: UGen -> UGen -> UGen -> UGen
bufDelayN buf i dly = mkFilter "BufDelayN" [buf, i, dly] 1

-- * Buffer I\/O

-- | Buffer reader.
bufRd :: Int -> Rate -> UGen -> UGen -> Loop -> Interpolation -> UGen
bufRd n r buf phs lp intp = mkOsc r "BufRd" [buf, phs, from_loop lp, from_interpolation intp] n

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
bufWr buf phs lp i = mkFilterMCE "BufWr" [buf, phs, from_loop lp] i 0

-- | Search a buffer for a value.
detectIndex :: UGen -> UGen -> UGen
detectIndex b i = mkFilter "DetectIndex" [b, i] 1

-- | Index into table with signal.
index :: UGen -> UGen -> UGen
index b i = mkFilter "Index" [b, i] 1

-- | Interpolating search in ordered table.
indexInBetween :: UGen -> UGen -> UGen
indexInBetween b i = mkFilter "IndexInBetween" [b, i] 1

-- | Wavetable oscillator.
osc :: Rate -> UGen -> UGen -> UGen -> UGen
osc r bufnum freq phase = mkOsc r "Osc" [bufnum, freq, phase] 1

-- | Wavetable oscillator.
oscN :: Rate -> UGen -> UGen -> UGen -> UGen
oscN r bufnum freq phase = mkOsc r "OscN" [bufnum, freq, phase] 1

-- | Buffer playback.
playBuf :: Int -> Rate -> UGen -> UGen -> UGen -> UGen -> Loop -> DoneAction -> UGen
playBuf n rt b r t s l a = mkOsc rt "PlayBuf" [b, r, t, s, from_loop l, from_done_action a] n

-- | Buffer recording.
recordBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> Loop -> UGen -> DoneAction -> UGen -> UGen
recordBuf b o rl pl r l t a i = mkOscMCE AR "RecordBuf" [b, o, rl, pl, r, from_loop l, t, from_done_action a] i 0

-- | Triggered buffer shuffler (grain generator).
tGrains :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains n t b r c d p a i = mkFilter "TGrains" [t, b, r, c, d, p, a, i] n

-- | Three variable wavetable oscillator.
vOsc3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
vOsc3 r b f1 f2 f3 = mkOsc r "VOsc3" [b, f1, f2, f3] 1

-- | Variable wavetable oscillator.
vOsc :: Rate -> UGen -> UGen -> UGen -> UGen
vOsc r b f phase = mkOsc r "VOsc" [b, f, phase] 1

-- * Local buffers

-- | Allocate a buffer local to the synth.
localBuf :: ID i => i -> UGen -> UGen -> UGen
localBuf z nf nc = mkOscId z IR "LocalBuf" [nc, nf] 1
-- note that nf & nc are swapped at actual ugen

-- | Set the maximum number of local buffers in a synth.
maxLocalBufs :: UGen -> UGen
maxLocalBufs n = mkOsc IR "MaxLocalBufs" [n] 0

-- | Set local buffer values.
setBuf :: UGen -> [UGen] -> UGen -> UGen
setBuf b xs o = mkOsc IR "SetBuf" ([b, o, fromIntegral (length xs)] ++ xs) 1

-- | Generate a localBuf and use setBuf to initialise it.
asLocalBuf :: ID i => i -> [UGen] -> UGen
asLocalBuf i xs =
    let m = maxLocalBufs 8
        b = mrg2 (localBuf i (fromIntegral (length xs)) 1) m
        s = setBuf b xs 0
    in mrg2 b s

-- Local Variables:
-- truncate-lines:t
-- End:
