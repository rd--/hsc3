> Sound.SC3.UGen.Help.viewSC3Help "Operator.difsqr"
> :t difSqr

> import Sound.SC3

> let { a = fSinOsc AR 800 0
>     ; b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0 }
> in audition (out 0 (difSqr a b * 0.125))

Written out:
> let { a = fSinOsc AR 800 0
>     ; b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0 }
> in audition (out 0 ((a * a - b * b) * 0.125))
