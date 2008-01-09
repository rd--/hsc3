peak trig reset

Outputs the maximum value read at the `trig' input until `reset' is
triggered.

> do { t <- dust AR 20
>    ; let { r = impulse AR 0.4 0
>          ; f = peak t r * 500 + 200 }
>      in audition (out 0 (sinOsc AR f 0 * 0.2)) }
