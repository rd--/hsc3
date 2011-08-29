> Sound.SC3.UGen.Help.viewSC3Help "VarSaw"
> Sound.SC3.UGen.DB.ugenSummary "VarSaw"

> import Sound.SC3

> let {f = lfPulse KR (mce2 3 3.03) 0 0.3 * 200 + 200
>     ;w = linLin (lfTri KR 1 0) (-1) 1 0 1}
> in audition (out 0 (varSaw AR f 0 w * 0.1))

Compare with lfPulse at AR
> let f = lfPulse KR 3 0 0.3 * 200 + 200
> in audition (out 0 (mce [varSaw AR f 0 0.2
>                         ,lfPulse AR f 0 0.2] * 0.1))
