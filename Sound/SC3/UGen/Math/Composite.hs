module Sound.SC3.UGen.Math.Composite where

import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

ugen_if :: Num a => a -> a -> a -> a
ugen_if p q r = (p * q) + ((1 - p) * r)

-- > ugen_integral_and_fractional_parts 1.5 == mce2 1 0.5
ugen_integral_and_fractional_parts :: UGen -> UGen
ugen_integral_and_fractional_parts n =
    let gt_eq_0 = let n' = floorE n in mce2 n' (n - n')
        lt_0 = let n' = ceilingE n in mce2 n' (n - n')
    in ugen_if (n >=* 0) gt_eq_0 lt_0

ugen_fmidi_to_midi_detune :: UGen -> UGen
ugen_fmidi_to_midi_detune mnn =
    let (n,c) = unmce2 (ugen_integral_and_fractional_parts mnn)
    in mce2 n (c * 100)
