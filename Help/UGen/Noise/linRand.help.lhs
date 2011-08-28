> Sound.SC3.UGen.Help.viewSC3Help "LinRand"
> Sound.SC3.UGen.DB.ugenSummary "LinRand"

> import Sound.SC3.ID

> let {f = linRand 'a' 200.0 10000.0 (mce [-1, 1])
>     ;e = line KR 0.4 0 0.01 RemoveSynth}
> in audition (out 0 (fSinOsc AR f 0 * e))
