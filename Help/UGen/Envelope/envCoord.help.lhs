> import Sound.SC3
> :t envCoord

co-ordinate (break-point) envelope
> let {c = EnvLin
>     ;p = envCoord [(0,0),(0.5,0.1),(0.55,1),(1,0)] 9 0.1 c
>     ;e = envGen KR 1 1 0 1 RemoveSynth p}
> in audition (out 0 (sinOsc AR 440 0 * e))

line segments, set target value & transition time and trigger
> let {tr = tr_control "tr" 1
>     ;st = control KR "st" 440
>     ;en = control KR "en" 880
>     ;tm = control KR "tm" 2
>     ;p = envCoord [(0,st),(tm,en)] 1 1 EnvLin
>     ;e = envGen KR tr 1 0 1 DoNothing p}
> in audition (out 0 (sinOsc AR e 0 * 0.2))

> withSC3 (send (n_set (-1) [("en",550),("tm",4),("tr",1)]))
> withSC3 (send (n_set (-1) [("en",990),("tm",1),("tr",1)]))
> withSC3 (send (n_set (-1) [("en",110),("tm",2),("tr",1)]))

plotting
> import Sound.SC3.Plot

> let {c0 = [(0,0),(0.35,0.1),(0.55,1),(1,0)]
>     ;c1 = [(0,0),(0.15,0.6),(0.35,0.2),(1,0)]}
> in plotEnvelope [envCoord c0 9 0.1 EnvLin,envCoord c1 6 0.1 EnvLin]

