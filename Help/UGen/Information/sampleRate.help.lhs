sampleRate

Server sample rate.

Compare a sine tone derived from sample rate with a 440Hz tone.

> let f = Mce [sampleRate IR * 0.01, 440]
> audition $ sinOsc AR f 0 * 0.1
