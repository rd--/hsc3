    Sound.SC3.UGen.Help.viewSC3Help "DynKlang"
    Sound.SC3.UGen.DB.ugenSummary "DynKlang"

> import Sound.SC3 {- hsc3 -}

fixed

> g_01 =
>     let s = klangSpec [800,1000,1200] [0.3,0.3,0.3] [pi,pi,pi]
>     in dynKlang AR 1 0 s * 0.4

fixed: randomised

> g_02 =
>     let f = map (\z -> rand z 600 1000) ['a'..'l']
>         s = klangSpec f (replicate 12 1) (replicate 12 0)
>     in dynKlang AR 1 0 s * 0.05

dynamic: frequency modulation

> g_03 =
>     let f = mce3 800 1000 1200 + sinOsc KR (mce3 2 3 4.2) 0 * mce3 13 24 12
>         a = mce3 0.3 0.3 0.3
>         p = mce3 pi pi pi
>     in dynKlang AR 1 0 (klangSpec_mce f a p) * 0.1
