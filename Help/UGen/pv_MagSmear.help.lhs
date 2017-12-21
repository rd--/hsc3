    Sound.SC3.UGen.Help.viewSC3Help "PV_MagSmear"
    Sound.SC3.UGen.DB.ugenSummary "PV_MagSmear"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let i = soundIn 0
>       c = fft' (localBuf 'α' 2048 1) i
>       x = mouseX KR 0 100 Linear 0.2
>       h = pv_MagSmear c x
>   in ifft' h * 0.5
