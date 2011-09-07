> Sound.SC3.UGen.Help.viewSC3Help "Operator.ring1"
> :t ring1

> import Sound.SC3

> let { a = fSinOsc AR 800 0
>     ; b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0 }
> in audition (out 0 (ring1 a b * 0.125))

is equivalent to:
> let { a = fSinOsc AR 800 0
>     ; b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0 }
> in audition (out 0 (((a * b) + a) * 0.125))
