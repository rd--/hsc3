> Sound.SC3.UGen.Help.viewSC3Help "FreeSelf"
> Sound.SC3.UGen.DB.ugenSummary "FreeSelf"

> import Sound.SC3

> let {n = dust 'α' KR 0.5
>     ;a = freeSelf n
>     ;b = out 0 (sinOsc AR 440 0 * 0.1)}
> in audition (mrg [a,b])
