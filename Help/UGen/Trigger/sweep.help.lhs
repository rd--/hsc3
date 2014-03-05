> Sound.SC3.UGen.Help.viewSC3Help "Sweep"
> Sound.SC3.UGen.DB.ugenSummary "Sweep"

> import Sound.SC3.ID

Using sweep to modulate sine frequency

> let {x = mouseX KR 0.5 20 Exponential 0.1
>     ;t = impulse KR x 0
>     ;f = sweep t 700 + 500}
> in audition (out 0 (sinOsc AR f 0 * 0.2))

Load audio to buffer

> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (send (b_allocRead 0 fn 0 0))

Using sweep to index into a buffer

> let {x = mouseX KR 0.5 20 Exponential 0.1
>     ;t = impulse AR x 0
>     ;p = sweep t (bufSampleRate KR 0)}
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))

Backwards, variable offset

> let {n = lfNoise0 'a' KR 15
>     ;x = mouseX KR 0.5 10 Exponential 0.1
>     ;t = impulse AR x 0
>     ;r = bufSampleRate KR 0
>     ;p = sweep t (negate r) + (bufFrames KR 0 * n)}
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))

Raising rate

> let {x = mouseX KR 0.5 10 Exponential 0.1
>     ;t = impulse AR x 0
>     ;r = sweep t 2 + 0.5
>     ;p = sweep t (bufSampleRate KR 0 * r)}
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))

f0 (sc-users, 2012-02-09)

> let {lf = range 0.01 1.25 (lfNoise2 'a' KR 1)
>     ;du = duty AR lf 0 DoNothing lf
>     ;tr = abs (hpz1 du) >* 0
>     ;ph = sweep tr (1/du)
>     ;fr = linExp ph 0 1 400 800
>     ;os = sinOsc AR fr 0 * 0.2}
> in audition (out 0 os)

line segments, set start & end values, transition time and trigger.
continues past end point if not re-triggered.

> let {tr = tr_control "tr" 0
>     ;st = control KR "st" 440
>     ;en = control KR "en" 880
>     ;tm = control KR "tm" 2
>     ;rt = ((en - st) / tm)
>     ;sw = sweep tr rt + st}
> in audition (out 0 (sinOsc AR sw 0 * 0.2))

> withSC3 (send (n_set (-1) [("st",660),("en",550),("tm",4),("tr",1)]))
> withSC3 (send (n_set (-1) [("st",110),("en",990),("tm",1),("tr",1)]))
