dc rate value

Create a constant amplitude signal.  This UGen simply outputs the
initial value you give it.

  value - constant value to output, cannot be modulated

> import Sound.SC3

won't work (the output is 0.5*0.0), which is why we need the DC UGen!

> audition (out 0 0.5)

constantly zero

> audition (out 0 (dc AR 0.5))

DC offset; will click on start and finish

> audition (out 0 (0.5 + sinOsc AR 440 0 * 0.1))
> audition (out 0 (dc AR 0.5 + sinOsc AR 440 0 * 0.1))

test - note the transient before LeakDC adapts and suppresses the offset

> audition (out 0 (dc AR 1))
> audition (out 0 (leakDC (dc AR 1) 0.995))

