freeSelf src

Free enclosing synth when the input signal crosses from non-positive
to positive.

> let { a = freeSelf (mouseX KR (-1) 1 Linear 0.1)
>     ; b = out 0 (sinOsc AR 440 0 * 0.1) }
> in audition (mrg [a, b])
