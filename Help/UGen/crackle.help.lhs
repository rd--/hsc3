    Sound.SC3.UGen.Help.viewSC3Help "Crackle"
    Sound.SC3.UGen.DB.ugenSummary "Crackle"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = crackle AR 1.95 * 0.2

Modulate chaos parameter

> g_02 = crackle AR (line KR 1.0 2.0 3 RemoveSynth) * 0.2

Drawing

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.01 (crackle AR 1.95)
    > plot_ugen1 0.025 (crackle AR (line KR 1.0 2.0 0.025 DoNothing))

![](sw/hsc3/Help/SVG/crackle.0.svg)
![](sw/hsc3/Help/SVG/crackle.1.svg)
