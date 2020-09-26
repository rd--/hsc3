> import Sound.SC3 {- hsc3 -}

distorts of course - would need scaling

> g_01 = runningSum (soundIn 0 * 0.1) 40

Running Average over x samples

> g_02 = let x = 100 in runningSum (lfSaw AR 440 0 * 0.1) x * recip x

RMS Power

> g_03 =
>     let input = lfSaw AR 440 0 * 0.1
>         numsamp = 30
>     in runningSum (input * input) numsamp / (sqrt numsamp)

composite UGen

> g_04 = runningSumRMS (soundIn 0) 40 * 0.1

> g_05 =
>     let z = soundIn 0
>         a = runningSum z 40
>     in sinOsc AR 440 0 * a * 0.1
