    Sound.SC3.UGen.Help.viewSC3Help "PV_SpectralMap"
    Sound.SC3.UGen.DB.ugenSummary "PV_SpectralMap"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> n_01 = "/home/rohan/opt/src/supercollider/sounds/a11wlk01.wav"

> m_01 = b_allocRead 10 n_01 0 0

    withSC3 (async m_01)

> g_01 =
>   let freeze = mouseY KR (-1) 1 Linear 0.2
>       a = localBuf 'α' 2048 1
>       b = localBuf 'β' 2048 1
>       c1 = fft' a (soundIn 0)
>       c2 = fft' b (playBuf 1 AR 10 1 1 0 Loop DoNothing)
>       c3 = pv_SpectralMap c1 c2 0.0 freeze (mouseX KR (-1) 1 Linear 0.2) 1 0
>   in ifft' c3
