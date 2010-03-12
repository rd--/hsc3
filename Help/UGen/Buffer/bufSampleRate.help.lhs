bufSampleRate rate bufnum

Buffer sample rate.

> import Sound.SC3

> let fn = "/home/rohan/audio/metal.wav"
> in withSC3 (\fd -> async fd (b_allocRead 0 fn 0 0))

Compare a sine tone derived from sample rate of a buffer with a
440Hz tone.

> let f = mce [bufSampleRate KR 0 * 0.01, 440]
> in audition (out 0 (sinOsc AR f 0 * 0.1))
