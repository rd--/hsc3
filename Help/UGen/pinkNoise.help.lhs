    Sound.SC3.UGen.Help.viewSC3Help "PinkNoise"
    Sound.SC3.UGen.DB.ugenSummary "PinkNoise"

> import Sound.SC3 {- hsc3 -}

    audition . (out 0) . (* 0.05) =<< pinkNoiseM AR
    audition . (out 0) . (* 0.05) =<< whiteNoiseM AR
    audition . (out 0) . (* 0.05) =<< brownNoiseM AR

speaker balance

> g_01 = let n = pinkNoise 'α' AR * 0.05 in mce2 n n
>
> g_02 =
>     let x = mouseX KR 0 1 Linear 0.2
>         x' = 1 - x
>         n = pinkNoise 'α' AR * 0.05
>     in mce2 (n * x') (n * x)

Drawing

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.1 (pinkNoise 'α' AR)

    import Sound.SC3.Plot.FFT {- hsc3-plot -}
    plot_ugen_fft1 0.1 (pinkNoise 'α' AR)

![](sw/hsc3/Help/SVG/pinkNoise.0.svg)
![](sw/hsc3/Help/SVG/pinkNoise.1.svg)
