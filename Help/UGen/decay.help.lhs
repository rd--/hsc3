    Sound.SC3.UGen.Help.viewSC3Help "Decay"
    Sound.SC3.UGen.DB.ugenSummary "Decay"

> import Sound.SC3 {- hsc3 -}

Used as an envelope.

> g_01 =
>     let n = pinkNoise 'a' AR
>         s = impulse AR (xLine KR 1 50 20 RemoveSynth) 0.25
>     in decay s 0.2 * n

Drawing

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.05 (decay (impulse AR 1 0) 0.01)

![](sw/hsc3/Help/SVG/decay.0.svg)
