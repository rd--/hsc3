    Sound.SC3.UGen.Help.viewSC3Help "PV_Morph"
    Sound.SC3.UGen.DB.ugenSummary "PV_Morph"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let o1 = pulse AR 180 (lfCub KR 1 0 * 0.1 + 0.3) * 0.5
>       o2 = varSaw AR 190 0 (lfCub KR 0.8 0 * 0.4 + 0.5) * 0.5
>       c1 = fft' (localBuf 'α' 2048 1) o1
>       c2 = fft' (localBuf 'β' 2048 1) o2
>       x = mouseX KR 0 1 Linear 0.2
>       h = pv_Morph c1 c2 x
>   in ifft' h * 0.5
