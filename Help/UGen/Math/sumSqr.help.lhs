sumSqr a b

Return the value of (a*a) + (b*b). This is more efficient than
using separate unit generators for each operation.

> let a = fSinOsc AR 800 0
>     b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0
> audition (out 0 (sumSqr a b * 0.125))

Written out:

> let a = fSinOsc AR 800 0
>     b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0
> audition (out 0 ((a * a + b * b) * 0.125))
