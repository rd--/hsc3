> Sound.SC3.UGen.Help.viewSC3Help "ReplaceOut"
> Sound.SC3.UGen.DB.ugenSummary "ReplaceOut"

> import Sound.SC3

Send signal to a bus, overwrite existing signal.
> let {a = out 0 (sinOsc AR (mce [330, 331]) 0 * 0.1)
>     ;b = replaceOut 0 (sinOsc AR (mce [880, 881]) 0 * 0.1)
>     ;c = out 0 (sinOsc AR (mce [120, 121]) 0 * 0.1)}
> in audition (mrg [a, b, c])

Compare to
> let {a = out 0 (sinOsc AR (mce [330, 331]) 0 * 0.1)
>     ;b = out 0 (sinOsc AR (mce [880, 881]) 0 * 0.1)
>     ;c = out 0 (sinOsc AR (mce [120, 121]) 0 * 0.1)}
> in audition (mrg [a, b, c])
