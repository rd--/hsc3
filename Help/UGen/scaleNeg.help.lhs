> import Sound.SC3 {- hsc3 -}

> g_01 = scaleNeg (fSinOsc AR 500 0) (line AR 1 (-1) 4 RemoveSynth) * 0.1

written out:

> g_02 =
>     let o = fSinOsc AR 500 0
>         l = line AR 1 (-1) 4 RemoveSynth
>         c = o <** 0
>     in (c * (o * l) + (1 - c) * o) * 0.1
