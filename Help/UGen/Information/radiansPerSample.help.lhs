> Sound.SC3.UGen.Help.viewSC3Help "RadiansPerSample"
> Sound.SC3.UGen.DB.ugenSummary "RadiansPerSample"

> import Sound.SC3

two pi divided by the nominal sample rate (ie. a very small number)
> let f = mce2 radiansPerSample ((2 * pi) / sampleRate) * 5e6
> in audition (out 0 (sinOsc AR f 0 * 0.1))
