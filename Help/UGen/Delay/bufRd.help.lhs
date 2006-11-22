bufrd numChannels bufnum phase loop interpolation

Plays the content of a buffer.

The number of channels must be a fixed integer. The architechture
of the SynthDef cannot change after it is compiled. NOTE: if you
supply a bufnum of a buffer that has a different numChannels then
you have specified to the BufRd, it will fail silently.

The interpolation type is an integer: 1 no interpolation, 2 linear
interpolation, 4 cubic interpolation.

> send' sc (b_allocRead 0 (resolve "audio/metal.wav"))

> bufrd AR 1 0 (sinosc AR 0.1 * bufframes IR 0) 0 0

> bufrd AR 1 0 (lfnoise1 0 AR 1 * bufframes IR 0)
