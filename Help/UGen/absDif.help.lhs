    Sound.SC3.UGen.Help.viewSC3Help "Operator.absdif"
    :t absDif

> import Sound.SC3 {- hsc3 -}

Finding the magnitude of the difference of two values is a common operation.

> g_01 =
>     let p = fSinOsc AR 440 0
>         q = fSinOsc AR 2 0 * 0.5
>     in p * 0.2 `absDif` q
