    Sound.SC3.UGen.Help.viewSC3Help "TPV"
    Sound.SC3.UGen.DB.ugenSummary "TPV"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins {- hsc3 -}

> fft_sz = 2048::Int
> hop_sz = fft_sz `div` 2
> fn_0 = "/home/rohan/data/audio/pf-c5.snd"
> fn_1 = "/home/rohan/data/audio/material/tyndall/var/talking-fragments/0001.WAV"
> tpv' b i = tpv (fft b i 0.5 1 1 0) (constant fft_sz) (constant hop_sz)

    > withSC3 (do {_ <- async (b_alloc 0 fft_sz 1)
    >             ;async (b_allocRead 1 fn_1 0 0)})

> g_01 =
>     let i = playBuf 1 AR 1 (bufRateScale KR 1) 1 0 Loop DoNothing
>         x = mouseX KR 1 70 Linear 0.1
>         y = mouseY KR 0.25 3 Linear 0.1
>         o = tpv' 0 i 70 x y 4 0.2
>     in mce2 (i * 0.1) o

> g_02 =
>     let i = playBuf 1 AR 1 (bufRateScale KR 1) 1 0 Loop DoNothing
>         x = mouseX KR 0.1 100 Linear 0.1
>         y = mouseY KR (-20) 40 Linear 0.1
>         o = tpv' 0 i 50 50 1 x (dbAmp y)
>     in pan2 o 0 1
