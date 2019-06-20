> import Sound.SC3 {- hsc3 -}

> g_01 = quadC AR (sampleRate / 2) 1 (-1) (-0.75) 0 * 0.2

> g_02 =
>   let x = mouseX KR 3.5441 4 Linear 0.1
>   in quadC AR 4000 (negate x) x 0 0.1 * 0.4

> g_03 =
>   let x = mouseX KR 3.5441 4 Linear 0.1
>       f = quadC AR 4 (negate x) x 0 0.1 * 800 + 900
>   in sinOsc AR f 0 * 0.4
