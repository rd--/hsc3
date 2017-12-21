    Sound.SC3.UGen.Help.viewSC3Help "PV_Invert"
    Sound.SC3.UGen.DB.ugenSummary "PV_Invert"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let s = sinOsc AR 440 0 * 0.4
>       n = pinkNoise 'α' AR * 0.1
>   in s + n

> f_01 z =
>   let c0 = fft' (localBuf 'β' 2048 1) z
>       c1 = pv_Invert c0
>   in mce2 z (ifft' c1) * 0.5

> g_02 = f_01 g_01

> g_03 = f_01 (soundIn 0)
