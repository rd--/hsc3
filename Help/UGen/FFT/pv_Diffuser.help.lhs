    Sound.SC3.UGen.Help.viewSC3Help "PV_Diffuser"
    Sound.SC3.UGen.DB.ugenSummary "PV_Diffuser"

> import Sound.SC3 {- hsc3 -}

> diff_01 = do
>   let fn = "/home/rohan/data/audio/pf-c5.snd"
>   withSC3 (async (b_alloc 10 2048 1) >> async (b_allocRead 12 fn 0 0))

> diff_02 = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing

> diff_02' = soundIn 0

Trigger revised phase shifts with MouseX crossing center of screen

> diff_03 =
>     let f = fft' 10 diff_02
>         x = mouseX KR 0 1 Linear 0.1
>         h = pv_Diffuser f (x >* 0.5)
>     in ifft' h * 0.5
