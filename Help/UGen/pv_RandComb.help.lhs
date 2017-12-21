    Sound.SC3.UGen.Help.viewSC3Help "PV_RandComb"
    Sound.SC3.UGen.DB.ugenSummary "PV_RandComb"

> import Sound.SC3 {- hsc3 -}

noise signal

> g_01 = whiteNoise 'α' AR * 0.5

outside world

> g_02 = soundIn 0

processor

> f_01 z =
>   let t = impulse KR 0.1 0
>       x = mouseX KR 0.6 0.95 Linear 0.1
>       c = pv_RandComb 'α' (fft' (localBuf 'α' 2048 1) z) x t
>   in pan2 (ifft' c) 0 1

> g_03 = f_01 g_02
