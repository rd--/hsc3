sampleRate

Server sample rate.

Compare a sine tone derived from sample rate with a 440Hz tone.

> let f = mce [sampleRate * 0.01, 440]
> in audition (out 0 (sinOsc AR f 0 * 0.1))

The server status command can extract nominal and
actual sample rates from a running server.

> withSC3 (\fd -> liftM2 (,)
>                        (serverSampleRateNominal fd) 
>                        (serverSampleRateActual fd))
