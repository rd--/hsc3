    Sound.SC3.UGen.Help.viewSC3Help "PV_RectComb"
    Sound.SC3.UGen.DB.ugenSummary "PV_RectComb"

> import Sound.SC3 {- hsc3 -}

noise source

> g_01 = whiteNoise 'α' AR * 0.3

outside world

> g_02 = soundIn 0

mouse control

> f_01 z =
>   let b = localBuf 'β' 2048 1
>       x = mouseX KR 0 0.5 Linear 0.1
>       y = mouseY KR 0 0.5 Linear 0.1
>       c = pv_RectComb (fft' b z) 8 x y
>   in pan2 (ifft' c) 0 1

> g_03 = f_01 g_02

lfo control

> f_02 z =
>   let b = localBuf 'γ' 2048 1
>       p = lfTri KR 0.097 0 *   0.4  + 0.5
>       w = lfTri KR 0.240 0 * (-0.5) + 0.5
>       c = pv_RectComb (fft' b z) 8 p w
>   in pan2 (ifft' c) 0 1

> g_04 = f_02 g_02
