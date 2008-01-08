sampleRate

Server sample rate.

Compare a sine tone derived from sample rate with a 440Hz tone.

> let f = mce [sampleRate * 0.01, 440]
> audition (out 0 (sinOsc AR f 0 * 0.1))
