linen gate attackTime susLevel releaseTime doneAction

A linear envelope generator.  The done flag is set when the
envelope reaches zero.

Note that the sustain level input is consulted only at the instant
when the gate is opened.

> import Sound.SC3

> let e = linen (impulse KR 2 0) 0.01 0.6 0.4 DoNothing
> in audition (out 0 (e * sinOsc AR 440 0 * 0.1))

> let { x = mouseX' KR (-1) 1 Linear 0.1
>     ; y = mouseY' KR 0.1 0.5 Linear 0.1
>     ; e = linen x 1 y 1.0 DoNothing }
> in audition (out 0 (sinOsc AR 440 0 * e))
