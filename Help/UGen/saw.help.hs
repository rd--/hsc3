-- saw ; SC3 saw is descending
saw ar (xLine kr 40 4000 6 RemoveSynth) * 0.1

-- saw ; negation is ascending
negate (saw ar (xLine kr 40 4000 6 RemoveSynth) * 0.1)

-- saw ; compare to the non-bandlimited lfSaw
lfSaw ar (xLine kr 40 4000 6 RemoveSynth) 0 * 0.1

-- saw ; two band limited sawtooth waves thru a resonant low pass filter
let f = xLine kr 8000 400 5 DoNothing
in rlpf (saw ar (mce2 100 250) * 0.1) f 0.05

-- saw ; not useful as a phasor, see lfSaw or phasor
sin (range 0 two_pi (negate (saw ar 440))) * 0.2

-- saw ; ln 2021-04-16 https://lukasnowok.github.io/spectrology/
let tr = impulse ar (xLine ar 1 1000 20 DoNothing) 0
    e = envGen ar tr 1 0 1 DoNothing (envPerc 0 0.1)
in saw ar (tRand 'a' 50 (xLine ar 50 5000 20 DoNothing) tr) * e * 0.1

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
Sound.SC3.Plot.plot_ugen1 0.1 (saw ar 50) -- descending
Sound.SC3.Plot.plot_ugen1 0.002 (saw ar 5000) -- ragged
