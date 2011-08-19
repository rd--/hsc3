bufRd numChannels rate bufnum phase loop interpolation

Plays the content of a buffer.

The number of channels must be a fixed integer. The architechture
of the SynthDef cannot change after it is compiled. NOTE: if you
supply a bufnum of a buffer that has a different numChannels then
you have specified to the BufRd, it will fail silently.

The interpolation value should be either NoInterpolation,
LinearInterpolation, CubicInterpolation or (Interpolation UGen).

> import Sound.SC3

> let fn = "/home/rohan/audio/metal.wav"
> in withSC3 (\fd -> send fd (b_allocRead 0 fn 0 0))

> let phase = (sinOsc AR 0.1 0 * bufFrames KR 0)
> in audition (out 0 (bufRd 1 AR 0 phase Loop NoInterpolation))

There are constructors, bufRdN, bufRdL, and bufRdC for the fixed
cases.

> import Sound.SC3.ID

> let { x = mouseX' KR (mce [5, 10]) 100 Linear 0.1
>     ; n = lfNoise1 'a' AR x }
> in audition (out 0 (bufRdL 1 AR 0 (n * bufFrames KR 0) Loop))
