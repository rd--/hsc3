runningMin in trig

Track maximum level.  Outputs the maximum value received at the
input.  When triggered, the maximum output value is reset to the
current value.

in   - input signal
trig - reset the output value to the current input value

> n <- dust AR 20
> let t = impulse AR 0.4 0
> audition $ sinOsc AR (runningMin n t * 500 + 200) 0 * 0.2

> let o = sinOsc KR 2 0
>     x = mouseX KR 0.01 10 Exponential 0.1
>     t = impulse AR x 0
> audition $ sinOsc AR (runningMin o t * 500 + 200) 0 * 0.2
