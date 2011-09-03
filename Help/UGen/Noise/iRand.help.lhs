> Sound.SC3.UGen.Help.viewSC3Help "IRand"
> Sound.SC3.UGen.DB.ugenSummary "IRand"

> import Sound.SC3.ID

> let {f = iRand 'a' 200 1200
>     ;e = line KR 0.2 0 0.1 RemoveSynth}
> in audition (out 0 (fSinOsc AR f 0 * e))
