> import Sound.SC3 {- hsc3 -}

SC3 saw is descending

> g_01 = saw AR (xLine KR 40 4000 6 RemoveSynth) * 0.1

negation is ascending

> g_02 = negate g_01

compare to the non-bandlimited lfSaw

> g_03 = lfSaw AR (xLine KR 40 4000 6 RemoveSynth) 0 * 0.1

Two band limited sawtooth waves thru a resonant low pass filter

> g_04 =
>     let f = xLine KR 8000 400 5 DoNothing
>     in rlpf (saw AR (mce2 100 250) * 0.1) f 0.05

saw is not useful as a phasor, see lfSaw or phasor

> g_05 = sin (range 0 two_pi (negate (saw AR 440))) * 0.2

Drawings

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (saw AR 50) -- descending
    > plot_ugen1 0.002 (saw AR 5000) -- ragged

![](sw/hsc3/Help/SVG/saw.0.svg)
![](sw/hsc3/Help/SVG/saw.1.svg)
