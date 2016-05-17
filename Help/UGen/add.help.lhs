    Sound.SC3.UGen.Help.viewSC3Help "Operator.+"
    :t (+)

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let o = fSinOsc AR 800 0
>         n = pinkNoise 'α' AR
>     in (o + n) * 0.1

DC offset.

> g_02 = fSinOsc AR 440 0 * 0.1 + 0.5
