pulseDivider trig div start

Outputs one impulse each time it receives a certain number of triggers
at its input.  A trigger happens when the signal changes from
non-positive to positive.

> let { p = impulse AR 8 0
>     ; d = pulseDivider p (mce [4,7]) 0
>     ; a = sinOsc AR 1200 0 * decay2 p 0.005 0.1
>     ; b = sinOsc AR 600  0 * decay2 d 0.005 0.5 }
> in audition (out 0 (a + b * 0.4))
