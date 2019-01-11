    Sound.SC3.UGen.Help.viewSC3Help "PV_Diffuser"
    Sound.SC3.UGen.DB.ugenSummary "PV_Diffuser"

> import Sound.SC3 {- hsc3 -}

> n_01 = "/home/rohan/data/audio/pf-c5.snd"

> m_01 = b_allocRead 12 n_01 0 0

    withSC3 (async m_01)

> g_01 = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing

> g_02 = soundIn 0

Trigger revised phase shifts with MouseX crossing center of screen

> f_01 z =
>   let f = fft' (localBuf 'α' 2048 1) z
>       x = mouseX KR 0 1 Linear 0.1
>       h = pv_Diffuser f (x >** 0.5)
>   in ifft' h * 0.5

> g_03 = f_01 g_02
