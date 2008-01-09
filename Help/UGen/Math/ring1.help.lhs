ring1 a b

Ring modulation plus first source.  Return the value of ((a*b) +
a). This is more efficient than using separate unit generators for the
multiply and add.

See also Mul, Ring1, Ring2, Ring3, Ring4.

> let { a = fSinOsc AR 800 0
>     ; b = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0 }
> in audition (out 0 (ring1 a b * 0.125))
