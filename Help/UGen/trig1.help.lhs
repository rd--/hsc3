> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let d = dust 'α' AR 1
>         o = fSinOsc AR 800 0 * 0.2
>     in o * trig1 d 0.2

> g_02 = sinOsc AR 440 0 * trig1 (impulse KR 10 0) 0.1 * 0.25
