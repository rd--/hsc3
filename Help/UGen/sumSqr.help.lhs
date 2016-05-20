    > Sound.SC3.UGen.Help.viewSC3Help "Operator.sumsqr"
    > :t sumSqr

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let a = fSinOsc AR 800 0
>         b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0
>     in sumSqr a b * 0.125

Written out:

> g_02 =
>     let a = fSinOsc AR 800 0
>         b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0
>     in (a * a + b * b) * 0.125
