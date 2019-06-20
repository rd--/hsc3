> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let x = mouseX KR 220 440 Exponential 0.1
>     in sinOsc AR (mce [x, lag3 x 1]) 0 * 0.1

> g_02 = lag3 (impulse AR 100 0) (mouseX KR 0.0 0.01 Linear 0.2)

> g_03 = lag3 (lfPulse AR 100 0 0.5) (mouseX KR 0.0 0.01 Linear 0.2)

> g_04 =
>   let x = mouseX KR 0.0 0.01 Linear 0.2
>   in lag (lag (lag (lfPulse AR 100 0 0.5 * 2 - 1) x) x) x
