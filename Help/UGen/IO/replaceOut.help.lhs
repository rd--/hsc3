> Sound.SC3.UGen.Help.viewSC3Help "ReplaceOut"
> Sound.SC3.UGen.DB.ugenSummary "ReplaceOut"

> import Sound.SC3.ID

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

a writes noise to 24
b reads 24 and replaces with filtered variant
c reads 24 and writes to 0

> let {a = out 24 (pinkNoise 'a' AR * 0.1)
>     ;b = replaceOut 24 (bpf (in' 1 AR 24) 440 1)
>     ;c = out 0 (in' 1 AR 24)}
> in mapM_ audition [a,b,c]
