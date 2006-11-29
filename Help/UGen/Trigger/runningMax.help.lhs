runningMax in trig

Track maximum level.  Outputs the maximum value received at the
input.  When triggered, the maximum output value is reset to the
current value.

in   - input signal
trig - reset the output value to the current input value

> n <- dust AR 20
> let t = impulse AR 0.4 0
> audition $ sinOsc AR (runningMax n t * 500 + 200) 0 * 0.2
