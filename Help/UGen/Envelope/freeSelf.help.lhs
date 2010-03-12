freeSelf src

Free enclosing synth when the input signal crosses from non-positive
to positive.

> import Sound.SC3.Monadic

> do { n <- dust KR 0.5
>    ; let { a = freeSelf n
>          ; b = out 0 (sinOsc AR 440 0 * 0.1) }
>      in audition (mrg [a, b]) }
