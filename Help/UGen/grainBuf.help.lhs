    Sound.SC3.UGen.Help.viewSC3Help "GrainBuf"
    Sound.SC3.UGen.DB.ugenSummary "GrainBuf"

> import Sound.SC3 {- hsc3 -}

> fn_01 = "/home/rohan/data/audio/pf-c5.snd"

    > withSC3 (async (b_allocRead 10 fn_01 0 0))

> g_01 =
>     let buf = 10
>         dur = 15
>         lin a b = line KR a b dur RemoveSynth
>         tr = impulse KR (lin 7.5 15) 0
>         gd = lin 0.05 0.1
>         r = lin 1 0.5 {- rate -}
>         i = lin 0 1 {- read-location -}
>         l = lin (-0.5) 0.5 {- stereo-location -}
>     in grainBuf 2 tr gd buf r i 2 l (-1) 512

> g_02 =
>     let b = 10
>         e = -1
>         x = mouseX KR (-1) 1 Linear 0.1
>         y = mouseY KR 10 45 Linear 0.1
>         i = impulse KR y 0
>         n1 = lfNoise1 'α' KR 500
>         n2 = lfNoise2 'β' KR 0.1
>         r = linLin n1 (-1) 1 0.5 2
>         p = linLin n2 (-1) 1 0 1
>     in grainBuf 2 i 0.1 b r p 2 x e 512
