    Sound.SC3.UGen.Help.viewSC3Help "DPW3Tri"
    Sound.SC3.UGen.DB.ugenSummary "DPW3Tri"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.External.RDU {- sc3-rdu -}

distortion creeps in under 200Hz

> g_01 = dPW3Tri AR (xLine KR 2000 20 10 DoNothing) * 0.1

very fast sweeps can have transient distortion effects

> g_02 = dPW3Tri AR (mouseX KR 200 12000 Exponential 0.2) * 0.2

compare

> g_03 = lfTri AR (mouseX KR 200 12000 Exponential 0.2) 0 * 0.1

less efficient than LFTri

> g_04 = let f = randN 50 'α' 50 5000
>        in splay (dPW3Tri AR f) 1 0.1 0 True

> g_05 = let f = randN 50 'α' 50 5000
>        in splay (lfTri AR f 0) 1 0.1 0 True

triangle is integration of square wave

> g_06 = let f = mouseX KR 440 8800 Exponential 0.2
>            o = pulse AR f 0.5
>        in integrator o 0.99 * 0.05

differentiation of triangle is square

> g_07 = let f = mouseX KR 440 8800 Exponential 0.2
>            o = dPW3Tri AR f
>        in hpz1 (o * 2) * 0.25

compare

> g_08 = let f = mouseX KR 440 8800 Exponential 0.2
>        in pulse AR f 0.5 * 0.1
