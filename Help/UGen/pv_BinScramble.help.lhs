    Sound.SC3.UGen.Help.viewSC3Help "PV_BinScramble"
    Sound.SC3.UGen.DB.ugenSummary "PV_BinScramble"

> import Sound.SC3 {- hsc3 -}

> n_01 = "/home/rohan/data/audio/pf-c5.snd"

> m_01 = [b_alloc 10 2048 1,b_allocRead 12 n_01 0 0]

     withSC3 (mapM_ async m_01)

> g_01 =
>   let a = playBuf 1 AR 12 (bufRateScale KR 12) 1 0 Loop DoNothing
>       f = fft' 10 a
>       x = mouseX KR 0.0 1.0 Linear 0.1
>       y = mouseY KR 0.0 1.0 Linear 0.1
>       g = pv_BinScramble 'α' f x y (impulse KR 4 0)
>   in pan2 (ifft' g) 0 0.5

careful - feedback loop!

> g_02 =
>   let a = soundIn 0
>       f = fft' 10 a
>       x = mouseX KR 0.15 1 Linear 0.1
>       y = mouseY KR 0.15 1 Linear 0.1
>       i = impulse KR (lfNoise0 'α' KR 2 * 8 + 10) 0
>       g = pv_BinScramble 'β' f x y i
>       h = ifft' g
>   in pan2 h 0 1
