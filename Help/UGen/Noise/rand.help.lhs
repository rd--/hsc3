> Sound.SC3.UGen.Help.viewSC3Help "Rand"
> Sound.SC3.UGen.DB.ugenSummary "Rand"

> import Sound.SC3.ID

> let {f = rand 'a' 200 1200
>     ;l = rand 'b' (-1) 1
>     ;e = line KR 0.2 0 0.1 RemoveSynth
>     ;o = fSinOsc AR f 0}
> in audition (out 0 (pan2 (o * e) l 1))
