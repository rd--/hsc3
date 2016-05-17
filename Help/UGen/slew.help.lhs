    Sound.SC3.UGen.Help.viewSC3Help "Slew"
    Sound.SC3.UGen.DB.ugenSummary "Slew"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = let z = lfPulse AR 800 0 0.5 * 0.1 in mce2 z (slew z 4000 4000)
>
> g_02 = let z = saw AR 800 * 0.1 in mce2 z (slew z 400 400)

Drawings

    > import Sound.SC3.Plot {- hsc3-plot -}
    > let z = lfPulse AR 800 0 0.5
    > plot_ugen1 0.05 z
    > plot_ugen1 0.05 (slew z 4000 4000)
    > plot_ugen1 0.05 (slew z 500 500)

![](sw/hsc3/Help/SVG/slew.0.svg)
![](sw/hsc3/Help/SVG/slew.1.svg)
![](sw/hsc3/Help/SVG/slew.2.svg)
