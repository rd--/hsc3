    Sound.SC3.UGen.Help.viewSC3Help "BrownNoise"
    Sound.SC3.UGen.DB.ugenSummary "BrownNoise"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = brownNoise 'α' AR * 0.1
>
> g_02 =
>     let n = brownNoise 'α' KR
>     in sinOsc AR (linExp n (-1) 1 64 9600) 0 * 0.1

Drawing

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.1 (brownNoise 'γ' AR)

    import Sound.SC3.Plot.FFT {- hsc3-plot -}
    plot_ugen_fft1 0.1 (brownNoise 'γ' AR)

![](sw/hsc3/Help/SVG/brownNoise.0.svg)
![](sw/hsc3/Help/SVG/brownNoise.1.svg)
