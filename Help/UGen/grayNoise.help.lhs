    Sound.SC3.UGen.Help.viewSC3Help "GrayNoise"
    Sound.SC3.UGen.DB.ugenSummary "GrayNoise"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = grayNoise 'α' AR * 0.1

Drawing

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.025 (grayNoise 'γ' AR)

    import Sound.SC3.Plot.FFT {- hsc3-plot -}
    plot_ugen_fft1 0.1 (grayNoise 'γ' AR)

![](sw/hsc3/Help/SVG/grayNoise.0.svg)
![](sw/hsc3/Help/SVG/grayNoise.1.svg)
