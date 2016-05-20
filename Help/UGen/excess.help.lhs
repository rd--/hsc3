    > Sound.SC3.UGen.Help.viewSC3Help "Operator.excess"
    > :t excess

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let o = fSinOsc AR 1000 0
>         l = line KR 0 1 8 DoNothing
>     in excess o l * 0.1

or written out in terms of clip2

> g_02 =
>     let o = fSinOsc AR 1000 0
>         l = line KR 0 1 8 DoNothing
>     in (o - clip2 o l) * 0.1
