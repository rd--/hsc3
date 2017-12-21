    Sound.SC3.UGen.Help.viewSC3Help "PV_BinWipe"
    Sound.SC3.UGen.DB.ugenSummary "PV_BinWipe"

> import Sound.SC3 {- hsc3 -}

> n_01 = "/home/rohan/data/audio/pf-c5.snd"

    withSC3 (async (b_allocRead 12 n_01 0 0))

> g_01 = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing

> g_02 = soundIn 0

> f_01 z =
>   let n = whiteNoise 'α' AR * 0.2
>       f = fft' (localBuf 'β' 2048 1) n
>       g = fft' (localBuf 'γ' 2048 1) z
>       x = mouseX KR 0.0 1.0 Linear 0.1
>       h = pv_BinWipe f g x
>   in pan2 (ifft' h) 0 0.5

> g_03 = f_01 g_01

> g_04 = f_01 g_02
