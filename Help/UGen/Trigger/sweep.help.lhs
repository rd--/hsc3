> Sound.SC3.UGen.Help.viewSC3Help "TDelay"
> Sound.SC3.UGen.DB.ugenSummary "TDelay"

> import Sound.SC3.ID

Using sweep to modulate sine frequency
> let {x = mouseX' KR 0.5 20 Exponential 0.1
>     ;t = impulse KR x 0
>     ;f = sweep t 700 + 500}
> in audition (out 0 (sinOsc AR f 0 * 0.2))

Load audio to buffer
> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (\fd -> send fd (b_allocRead 0 fn 0 0))

Using sweep to index into a buffer
> let {x = mouseX' KR 0.5 20 Exponential 0.1
>     ;t = impulse AR x 0
>     ;p = sweep t (bufSampleRate KR 0)}
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))

Backwards, variable offset
> let {n = lfNoise0 'a' KR 15
>     ;x = mouseX' KR 0.5 10 Exponential 0.1
>     ;t = impulse AR x 0
>     ;r = bufSampleRate KR 0
>     ;p = sweep t (negate r) + (bufFrames KR 0 * n)}
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))

Raising rate
> let {x = mouseX' KR 0.5 10 Exponential 0.1
>     ;t = impulse AR x 0
>     ;r = sweep t 2 + 0.5
>     ;p = sweep t (bufSampleRate KR 0 * r)}
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))
