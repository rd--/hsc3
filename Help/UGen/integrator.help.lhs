    > Sound.SC3.UGen.Help.viewSC3Help "Integrator"
    > Sound.SC3.UGen.DB.ugenSummary "Integrator"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let x = mouseX KR 0.001 0.999 Exponential 0.2
>         o = lfPulse AR 300 0.2 0.1 * 0.1
>     in integrator o x

used as an envelope

> g_02 =
>     let i = lfPulse AR 3 0.2 0.0004
>         o = sinOsc AR 700 0
>     in integrator i 0.999 * o


scope

> g_03 =
>     let x = mouseX KR 0.01 0.999 Exponential 0.2
>         o = lfPulse AR (1500 / 4) 0.2 0.1
>     in integrator o x * 0.1

Drawing

    > import Sound.SC3.Plot {- hsc3-plot -}
    > let s = lfPulse AR (1500 / 4) 0.2 0.1
    > plot_ugen 0.006 (integrator s (mce [0.1,0.4,0.7]))

![](sw/hsc3/Help/SVG/integrator.0.svg)
