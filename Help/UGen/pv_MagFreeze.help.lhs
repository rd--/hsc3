    Sound.SC3.UGen.Help.viewSC3Help "PV_MagFreeze"
    Sound.SC3.UGen.DB.ugenSummary "PV_MagFreeze"

> import Sound.SC3 {- hsc3 -}

Load audio file.

> n_01 = "/home/rohan/data/audio/pf-c5.snd"

> m_01 = [b_allocRead 12 n_01 0 0]

    withSC3 (mapM_ async m_01)

File as signal...

> g_01 = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing

Synthesised signal...

> g_02 =
>   let o1 = sinOsc KR 0.08 0
>       o2 = sinOsc KR (squared (o1 * 6 + 6.2)) 0 * 100 + 800
>   in sinOsc AR o2 0

Outside world signal...

> g_03 = soundIn 0

Process (freeze) 'z'...

> f_01 z =
>   let f = fft' (localBuf 'α' 2048 1) z
>       x = mouseX KR 0 1 Linear 0.1
>       h = pv_MagFreeze f (x >* 0.5)
>   in ifft' h * 0.5

> g_04 = f_01 g_03
