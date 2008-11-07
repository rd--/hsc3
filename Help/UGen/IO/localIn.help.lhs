localIn numChannels rate

Define and read from buses local to a SynthDef

numChannels - the number of channels of local buses.

LocalIn defines buses that are local to the SynthDef. These are like
the global buses, but are more convenient if you want to implement a
self contained effect that uses a feedback processing loop.  There can
only be one audio rate and one control rate LocalIn per SynthDef.  The
audio can be written to the bus using LocalOut.

> import Sound.SC3

> do { n <- whiteNoise AR
>    ; let { a0 = decay (impulse AR 0.3 0) 0.1 * n * 0.2
>          ; a1 = localIn 2 AR + mce [a0, 0]
>          ; a2 = delayN a1 0.2 0.2 
>          ; a3 = mceEdit reverse a2 * 0.8 }
>      in audition (mrg [localOut a3, out 0 a2]) }
