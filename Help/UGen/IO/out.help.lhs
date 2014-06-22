> Sound.SC3.UGen.Help.viewSC3Help "Out"
> Sound.SC3.UGen.DB.ugenSummary "Out"

> import Sound.SC3

Oscillators at outputs zero (330) and one (331)

> audition (out 0 (sinOsc AR (mce2 330 331) 0 * 0.1))

out is summing, as opposed to replaceOut

> audition (mrg [out 0 (sinOsc AR (mce2 330 990) 0 * 0.1)
>               ,out 0 (sinOsc AR (mce2 331 991) 0 * 0.1)])
