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

-- saw ; ln 2021-04-16 https://lukasnowok.github.io/spectrology/
let tr = impulse AR (xLine AR 1 1000 20 DoNothing) 0
    e = envGen AR tr 1 0 1 DoNothing (envPerc 0 0.1)
in saw AR (tRand 'a' 50 (xLine AR 50 5000 20 DoNothing) tr) * e * 0.1

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
Sound.SC3.Plot.plot_ugen1 0.1 (saw AR 50) -- descending
Sound.SC3.Plot.plot_ugen1 0.002 (saw AR 5000) -- ragged
