> Sound.SC3.UGen.Help.viewSC3Help "Amplitude"
> Sound.SC3.UGen.DB.ugenSummary "Amplitude"

> import Sound.SC3

> let {s = soundIn 4
>     ;a = amplitude KR s 0.01 0.01}
> in audition (out 0 (pulse AR 90 0.3 * a))

> let {s = soundIn 4
>     ;f = amplitude KR s 0.5 0.5 * 1200 + 400}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

> let {s = soundIn 4
>     ;a = amplitude AR s 0.5 0.05}
> in audition (out 0 (s * a))
