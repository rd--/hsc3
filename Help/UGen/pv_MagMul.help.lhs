    Sound.SC3.UGen.Help.viewSC3Help "PV_MagMul"
    Sound.SC3.UGen.DB.ugenSummary "PV_MagMul"

> import Sound.SC3 {- hsc3 -}

> f_01 z =
>     let y = lfSaw AR (midiCPS 43) 0 * 0.2
>         c0 = fft' (localBuf 'α' 2048 1) y
>         c1 = fft' (localBuf 'β' 2048 1) z
>         c2 = pv_MagMul c0 c1
>     in ifft' c2 * 0.1

> g_01 = f_01 (whiteNoise 'γ' AR * 0.2)

> g_02 = f_01 (soundIn 0 * 0.5)
