runningMax in trig

Track maximum level.  Outputs the maximum value received at the
input.  When triggered, the maximum output value is reset to the
current value.

in   - input signal
trig - reset the output value to the current input value

> do { n <- dust AR 20
>    ; let { t = impulse AR 0.4 0
>          ; f = runningMax n t * 500 + 200 }
>      in audition (out 0 (sinOsc AR f 0 * 0.2)) }
