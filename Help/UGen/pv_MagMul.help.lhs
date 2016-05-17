    Sound.SC3.UGen.Help.viewSC3Help "PV_MagMul"
    Sound.SC3.UGen.DB.ugenSummary "PV_MagMul"

> import Sound.SC3

    > withSC3 (async (b_alloc 0 2048 1) >> async (b_alloc 1 2048 1))

> mm0 z =
>     let y = lfSaw AR (midiCPS 43) 0 * 0.2
>         c0 = fft' (localBuf 'α' 2048 1) y
>         c1 = fft' (localBuf 'β' 2048 1) z
>         c2 = pv_MagMul c0 c1
>     in ifft' c2 * 0.1

> g_01 = mm0 (whiteNoise 'γ' AR * 0.2)
> g_02 = mm0 (soundIn 4 * 0.5)
