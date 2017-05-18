    :t envCoord

> import Sound.SC3

co-ordinate (break-point) envelope

> g_01 =
>     let c = EnvLin
>         p = envCoord [(0,0),(0.5,0.1),(0.55,1),(1,0)] 9 0.1 c
>         e = envGen KR 1 1 0 1 RemoveSynth p
>     in sinOsc AR 440 0 * e

line segments, set target value & transition time and trigger

> g_02 =
>     let tr = tr_control "tr" 1
>         st = control KR "st" 440
>         en = control KR "en" 880
>         tm = control KR "tm" 2
>         p = envCoord [(0,st),(tm,en)] 1 1 EnvLin
>         e = envGen KR tr 1 0 1 DoNothing p
>     in sinOsc AR e 0 * 0.2

    import Sound.OSC
    withSC3 (sendMessage (n_set (-1) [("en",550),("tm",4),("tr",1)]))
    withSC3 (sendMessage (n_set (-1) [("en",990),("tm",1),("tr",1)]))
    withSC3 (sendMessage (n_set (-1) [("en",110),("tm",2),("tr",1)]))

likewise, but internal graph triggers and randomises line end points

> g_03 =
>     let tr = dust 'α' KR 2
>         st = 440
>         en = tRand 'β' 300 900 tr
>         tm = tRand 'γ' 0.5 1.5 tr
>         p = envCoord [(0,st),(tm,en)] 1 1 EnvLin
>         e = envGen KR tr 1 0 1 DoNothing p
>     in sinOsc AR e 0 * 0.2

plotting

> e_01 =
>     let c0 = [(0,0),(0.35,0.1),(0.55,1),(1,0)]
>         c1 = [(0,0),(0.15,0.6),(0.35,0.2),(1,0)]
>         c2 = [(0,0),(0.65,0.3),(0.85,0.7),(1,0)]
>         c3 = [(0,0.1),(0.25,0.6),(0.5,0.4),(1,0.4)]
>     in [envCoord c0 9 0.1 EnvLin
>        ,envCoord c1 6 0.1 EnvSin
>        ,envCoord c2 5 0.1 EnvCub
>        ,envCoord c3 7 0.1 EnvStep]

    import Sound.SC3.Plot
    plotEnvelope e_01
