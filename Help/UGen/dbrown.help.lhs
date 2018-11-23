    Sound.SC3.UGen.Help.viewSC3Help "Dbrown"
    Sound.SC3.UGen.DB.ugenSummary "Dbrown"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let n = dbrown 'α' dinf 0 15 1 {- Dbrown(0, 15, 1, inf) -}
>       x = mouseX KR 1 40 Exponential 0.1
>       t = impulse KR x 0
>       f = demand t 0 n * 30 + 340
>   in sinOsc AR f 0 * 0.1

> g_02 =
>   let n = demand (impulse KR 10 0) 0 (dbrown 'α' dinf (-1) 1 0.05)
>       f = linExp n (-1) 1 64 9600
>   in sinOsc AR f 0 * 0.1
