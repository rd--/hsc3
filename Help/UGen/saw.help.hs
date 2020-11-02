-- saw ; SC3 saw is descending
saw AR (xLine KR 40 4000 6 RemoveSynth) * 0.1

-- saw ; negation is ascending
negate (saw AR (xLine KR 40 4000 6 RemoveSynth) * 0.1)

-- saw ; compare to the non-bandlimited lfSaw
lfSaw AR (xLine KR 40 4000 6 RemoveSynth) 0 * 0.1

-- saw ; two band limited sawtooth waves thru a resonant low pass filter
let f = xLine KR 8000 400 5 DoNothing
in rlpf (saw AR (mce2 100 250) * 0.1) f 0.05

-- saw ; not useful as a phasor, see lfSaw or phasor
sin (range 0 two_pi (negate (saw AR 440))) * 0.2

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (saw AR 50) -- descending
Sound.SC3.Plot.plot_ugen1 0.002 (saw AR 5000) -- ragged
