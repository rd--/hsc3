zeroCrossing in

Zero crossing frequency follower.

Outputs a frequency based upon the distance between interceptions of
the X axis. The X intercepts are determined via linear interpolation
so this gives better than just integer wavelength resolution. This is
a very crude pitch follower, but can be useful in some situations.

in - input signal.

> let a = sinOsc AR (sinOsc KR 1 0 * 600 + 700) 0 * 0.1
> audition $ MCE [a, impulse AR (zeroCrossing a) 0 * 0.25]
