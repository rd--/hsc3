    Sound.SC3.UGen.Help.viewSC3Help "LagUD"
    Sound.SC3.UGen.DB.ugenSummary "LagUD"

> import Sound.SC3 {- hsc3 -}

lag pitch, slower down (5 seconds) than up (1 second)

> g_01 =
>   let x = mouseX KR 220 440 Linear 0.2
>   in sinOsc AR (mce2 x (lagUD x 1 5)) 0 * 0.1

lag as signal filter

> f_01 s =
>   let x = mouseX KR 0.0001 0.01 Exponential 0.2
>       y = mouseY KR 0.0001 0.01 Exponential 0.2
>   in lagUD s x y

> g_02 = f_01 (0 - saw AR 440) * 0.15
> g_03 = f_01 (impulse AR (range 6 24 (lfNoise2 'α' KR 4)) 0) * 0.5

> g_04 =
>   let s = varSaw AR 220 0 (range 0 1 (sinOsc KR 0.25 0))
>   in f_01 s * 0.1

