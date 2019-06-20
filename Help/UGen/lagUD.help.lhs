> import Sound.SC3 {- hsc3 -}

lag pitch, slower down (5 seconds) than up (1 second)

> g_01 =
>   let x = mouseX KR 220 440 Linear 0.2
>   in sinOsc AR (mce2 x (lagUD x 1 5)) 0 * 0.1

as signal filter

> f_01 l s =
>   let x = mouseX KR 0.0001 0.01 Exponential 0.2
>       y = mouseY KR 0.0001 0.01 Exponential 0.2
>   in l s x y

> f_02 = f_01 lagUD
> f_03 = f_01 lag2UD
> f_04 = f_01 lag3UD

> g_02 = f_02 (0 - saw AR 440) * 0.15
> g_03 = f_02 (impulse AR (range 6 24 (lfNoise2 'α' KR 4)) 0) * 0.5
> g_04 = f_04 (lfPulse AR 800 0 0.5 * 2 - 1) * 0.25

> g_05 =
>   let s = varSaw AR 220 0 (range 0 1 (sinOsc KR 0.25 0))
>   in f_02 s * 0.1

> g_06 =
>   let x = mouseX KR 0.0 (1/100) Linear 0.2
>       y = mouseY KR 0.0 (3/100) Linear 0.2
>   in lagUD (lfPulse AR 50 0 0.25) x y * 0.2
