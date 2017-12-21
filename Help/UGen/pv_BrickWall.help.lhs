    Sound.SC3.UGen.Help.viewSC3Help "PV_BrickWall"
    Sound.SC3.UGen.DB.ugenSummary "PV_BrickWall"

> import Sound.SC3 {- hsc3 -}

> f_01 z =
>   let x = mouseX KR (-1) 1 Linear 0.1
>       c = fft' (localBuf 'α' 2048 1) z
>   in ifft' (pv_BrickWall c x)

> g_01 = f_01 (whiteNoise 'α' AR * 0.2)

> g_02 = f_01 (soundIn 0)

