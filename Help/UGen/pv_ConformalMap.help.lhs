    Sound.SC3.UGen.Help.viewSC3Help "PV_ConformalMap"
    Sound.SC3.UGen.DB.ugenSummary "PV_ConformalMap"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let i = soundIn 0
>       x = mouseX KR (-1) 1 Linear 0.1
>       y = mouseY KR (-1) 1 Linear 0.1
>   in pan2 (ifft' (pv_ConformalMap (fft' (localBuf 'α' 1024 1) i) x y)) 0 1

With filtering.

> f_01 z =
>   let x = mouseX KR 0.01  2.0 Linear 0.1
>       y = mouseY KR 0.01 10.0 Linear 0.1
>       c = fft' (localBuf 'β' 2048 1) z
>       m = ifft' (pv_ConformalMap c x y)
>   in pan2 (combN m 0.1 0.1 10 * 0.5 + m) 0 1

> g_02 =
>   let o = mce [1, 1.1, 1.5, 1.78, 2.45, 6.7, 8] * 220
>       f = sinOsc KR (mce [0.16, 0.33, 0.41]) 0 * 10 + o
>   in mix (lfSaw AR f 0) * 0.3

> g_03 = soundIn 0

> g_04 = f_01 g_02

> g_05 = f_01 g_03
