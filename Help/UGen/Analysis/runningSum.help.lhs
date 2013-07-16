> Sound.SC3.UGen.Help.viewSC3Help "RunningSum"
> Sound.SC3.UGen.DB.ugenSummary "RunningSum"

> import Sound.SC3

distorts of course - would need scaling
> audition (out 0 (runningSum (soundIn 4) 40))

Running Average over x samples
> let {x = 100
>     ;o = runningSum (lfSaw AR 440 0) x * recip x}
> in audition (out 0 o)

RMS Power
> let {input = lfSaw AR 440 0
>     ;numsamp = 30
>     ;o = runningSum (input * input) numsamp / (sqrt numsamp)}
> in audition (out 0 o)

composite UGen
> audition (out 0 (runningSumRMS (soundIn 4) 40))

> let {z = soundIn 4
>     ;a = runningSum z 40}
> in audition (out 0 (sinOsc AR 440 0 * a * 0.1))
