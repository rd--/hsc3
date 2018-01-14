    Sound.SC3.UGen.Help.viewSC3Help "LatoocarfianC"
    Sound.SC3.UGen.DB.ugenSummary "LatoocarfianC"

> import Sound.SC3

SC3 default initial parameters.

> g_01 =
>   let x = mouseX KR 20 sampleRate Linear 0.1
>   in latoocarfianC AR x 1 3 0.5 0.5 0.5 0.5 * 0.2

Randomly modulate all parameters.

> g_02 =
>   let [n0,n1,n2,n3] = map (\e -> lfNoise2 e KR 5) "abcd"
>       f = sampleRate / 4
>       a = n0 * 1.5 + 1.5
>       b = n1 * 1.5 + 1.5
>       c = n2 * 0.5 + 1.5
>       d = n3 * 0.5 + 1.5
>   in latoocarfianC AR f a b c d 0.5 0.5 * 0.2
