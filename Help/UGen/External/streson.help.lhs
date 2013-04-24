> Sound.SC3.UGen.Help.viewSC3Help "Streson"
> Sound.SC3.UGen.DB.ugenSummary "Streson"

> import Sound.SC3

> let {dt = recip (linExp (lfCub KR 0.1 (0.5 * pi)) (-1) 1 280 377)
>     ;s = streson (lfSaw AR (mce2 220 180) 0 * 0.2) dt 0.9 * 0.3}
> in audition (out 0 s)
