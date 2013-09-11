> Sound.SC3.UGen.Help.viewSC3Help "SampleRate"
> Sound.SC3.UGen.DB.ugenSummary "SampleRate"

> import Sound.SC3

the current nominal sample rate of the server
> let {sr = 48000 {- 44100 -}
>     ;f = mce2 sampleRate sr * 0.01}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

The server status command can extract nominal and actual sample rates
from a running server.
> import Control.Monad
> withSC3 (liftM2 (,) serverSampleRateNominal serverSampleRateActual)
