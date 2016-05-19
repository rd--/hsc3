    > Sound.SC3.UGen.Help.viewSC3Help "RunningSum"
    > Sound.SC3.UGen.DB.ugenSummary "RunningSum"

> import Sound.SC3 {- hsc3 -}

distorts of course - would need scaling

> g_01 = runningSum (soundIn 4) 40

Running Average over x samples

> g_02 = let x = 100 in runningSum (lfSaw AR 440 0) x * recip x

RMS Power

> g_03 =
>     let input = lfSaw AR 440 0
>         numsamp = 30
>     in runningSum (input * input) numsamp / (sqrt numsamp)

composite UGen

> g_04 = runningSumRMS (soundIn 4) 40

> g_05 =
>     let z = soundIn 4
>         a = runningSum z 40
>     in sinOsc AR 440 0 * a * 0.1
