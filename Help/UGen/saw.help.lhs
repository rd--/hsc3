    > Sound.SC3.UGen.Help.viewSC3Help "Saw"
    > Sound.SC3.UGen.DB.ugenSummary "Saw"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = saw AR (xLine KR 40 4000 6 RemoveSynth) * 0.1

compare to the non-bandlimited lfSaw

> g_02 = lfSaw AR (xLine KR 40 4000 6 RemoveSynth) 0 * 0.1

Two band limited sawtooth waves thru a resonant low pass filter

> g_03 =
>     let f = xLine KR 8000 400 5 DoNothing
>     in rlpf (saw AR (mce2 100 250) * 0.1) f 0.05

Drawings

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (saw AR 50) -- descending
    > plot_ugen1 0.002 (saw AR 5000) -- ragged

![](sw/hsc3/Help/SVG/saw.0.svg)
![](sw/hsc3/Help/SVG/saw.1.svg)
