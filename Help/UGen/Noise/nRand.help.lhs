> Sound.SC3.UGen.Help.viewSC3Help "NRand"
> Sound.SC3.UGen.DB.ugenSummary "NRand"

> import Sound.SC3

> let {n = nRand 'α' 1200.0 4000.0 (mce [2,5])
>     ;e = line KR 0.2 0 0.1 RemoveSynth}
> in audition (out 0 (fSinOsc AR n 0 * e))
