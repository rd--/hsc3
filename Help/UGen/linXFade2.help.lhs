    > Sound.SC3.UGen.Help.viewSC3Help "LinXFade2"
    > Sound.SC3.UGen.DB.ugenSummary "LinXFade2"

> import Sound.SC3 {- hsc3 -}

> g_01 = linXFade2 (saw AR 440) (sinOsc AR 440 0) (lfTri KR 0.1 0) 0.1

> f_01 ty = ty
>           (fSinOsc AR 800 0 * 0.2)
>           (pinkNoise 'α' AR * 0.2)
>           (fSinOsc KR 0.5 0)
>           1.0

> g_02 = f_01 linXFade2

> g_03 = f_01 xFade2
