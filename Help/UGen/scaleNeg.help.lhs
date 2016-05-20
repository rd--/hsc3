    > Sound.SC3.UGen.Help.viewSC3Help "Operator.scaleneg"
    > :t scaleNeg

> import Prelude hiding ((<*)) {- base -}
> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let o = fSinOsc AR 1000 0
>         l = line AR 1 (-1) 4 RemoveSynth
>     in scaleNeg o l * 0.1

written out:

> g_02 =
>     let o = fSinOsc AR 1000 0
>         l = line AR 1 (-1) 4 RemoveSynth
>         c = o <* 0
>     in (c * (o * l) + (1 - c) * o) * 0.1
