> Sound.SC3.UGen.Help.viewSC3Help "Amplitude"
> Sound.SC3.UGen.DB.ugenSummary "Amplitude"

> import Sound.SC3

> let {s = in' 1 AR numOutputBuses
>     ;a = amplitude KR s 0.1 0.1}
> in audition (out 0 (pulse AR 90 0.3 * a))

> let {s = in' 1 AR numOutputBuses
>     ;f = amplitude KR s 0.1 0.1 * 1200 + 400}
> in audition (out 0 (sinOsc AR f 0 * 0.3))
