    > Sound.SC3.UGen.Help.viewSC3Help "Operator.max"
    > :t max

> import Sound.SC3 {- hsc3 -}

q modulates and envelopes p

> g_01 =
>     let p = fSinOsc AR 500 0 * 0.25
>         q = fSinOsc AR 0.5 0
>     in p `max` q
