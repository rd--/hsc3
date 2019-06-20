> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let x = mouseX KR 220 440 Exponential 0.1
>     in sinOsc AR (mce [x, lag2 x 1]) 0 * 0.1

> g_02 = lag2 (impulse AR 100 0) (mouseX KR 0.0 0.01 Linear 0.2)
