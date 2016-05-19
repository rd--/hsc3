    > Sound.SC3.UGen.Help.viewSC3Help "RunningMin"
    > Sound.SC3.UGen.DB.ugenSummary "RunningMin"

> import Sound.SC3 {- hsc3 -}

Follow a sine lfo, reset rate controlled by mouseX

> g_01 =
>     let o = sinOsc KR 2 pi
>         x = mouseX KR 0.01 10 Exponential 0.1
>         t = impulse AR x 0
>         f = runningMin o t * 500 + 200
>     in t + sinOsc AR f 0 * 0.1
>
> g_02 =
>     let n = dust 'α' AR 20
>         t = impulse AR 0.4 0
>         f = runningMin n t * 500 + 200
>     in sinOsc AR f 0 * 0.2
