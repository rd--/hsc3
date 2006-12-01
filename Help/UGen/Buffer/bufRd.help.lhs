bufRd numChannels bufnum phase loop interpolation

Plays the content of a buffer.

The number of channels must be a fixed integer. The architechture
of the SynthDef cannot change after it is compiled. NOTE: if you
supply a bufnum of a buffer that has a different numChannels then
you have specified to the BufRd, it will fail silently.

The interpolation type is an integer: 1 no interpolation, 2 linear
interpolation, 4 cubic interpolation.

> withSC3 (\fd -> send fd (b_allocRead 0 "/home/rohan/sw/sw-01/audio/metal.wav" 0 0))

> audition $ bufRd 1 AR 0 (sinOsc AR 0.1 0 * bufFrames KR 0) 0 0

> let x = mouseX KR (MCE [5, 10]) 100 Linear 0.1
> n <- lfNoise1 AR x
> audition $ bufRd 1 AR 0 (n * bufFrames KR 0) 0 2
