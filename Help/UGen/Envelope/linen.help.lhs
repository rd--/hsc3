linen gate attackTime susLevel releaseTime doneAction

A linear envelope generator.  The done flag is set when the
envelope reaches zero.

Note that the sustain level input is consulted only at the instant
when the gate is opened.

> let e = linen (impulse KR 2 0) 0.01 0.6 0.4 DoNothing
> audition $ e * sinOsc AR 440 0 * 0.1

> let x = mouseX KR (-1) 1 Linear 0.1
>     y = mouseY KR 0.1 0.5 Linear 0.1
>     e = linen x 1 x 1.0 DoNothing
> audition $ sinOsc AR 440 0 * e
