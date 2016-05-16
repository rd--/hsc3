> Sound.SC3.UGen.Help.viewSC3Help "SampleDur"
> Sound.SC3.UGen.DB.ugenSummary "SampleDur"

> import Sound.SC3

the reciprocal of the nominal sample rate of the server

> let f = mce2 sampleRate (recip sampleDur) * 0.01
> in audition (out 0 (sinOsc AR f 0 * 0.1))
