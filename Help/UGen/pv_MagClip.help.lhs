    Sound.SC3.UGen.Help.viewSC3Help "PV_MagClip"
    Sound.SC3.UGen.DB.ugenSummary "PV_MagClip"

> import Sound.SC3

> g_01 =
>   let f = fft' (localBuf 'α' 2048 1) (soundIn 0)
>       c = 128
>       x = mouseX KR 0 c Linear 0.1
>       h = pv_MagClip f x
>   in ifft' h * 0.5
