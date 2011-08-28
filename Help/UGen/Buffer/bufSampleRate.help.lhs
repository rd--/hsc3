> Sound.SC3.UGen.Help.viewSC3Help "BufSampleRate"
> Sound.SC3.UGen.DB.ugenSummary "BufSampleRate"

> import Sound.SC3

Load sound file to buffer zero (required for examples)
> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (\fd -> async fd (b_allocRead 0 fn 0 0))

Sine tone derived from sample rate of buffer an 440Hz tone.
> let f = mce [bufSampleRate KR 0 * 0.01, 440]
> in audition (out 0 (sinOsc AR f 0 * 0.1))
