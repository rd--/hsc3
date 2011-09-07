> Sound.SC3.UGen.Help.viewSC3Help "Atari2600"
> Sound.SC3.UGen.DB.ugenSummary "Atari2600"

> import Sound.SC3

> audition (out 0 (atari2600 1 2 3 4 5 5 1))
> audition (out 0 (atari2600 2 3 10 10 5 5 1))

> let {x = mouseX' KR 0 15 Linear 0.1
>     ;y = mouseY' KR 0 15 Linear 0.1}
> in audition (out 0 (atari2600 x y 10 10 5 5 1))

> let {x = mouseX' KR 0 31 Linear 0.1
>     ;y = mouseY' KR 0 31 Linear 0.1}
> in audition (out 0 (atari2600 2 3 x y 5 5 1))

> let {x = mouseX' KR 0 15 Linear 0.1
>     ;y = mouseY' KR 0 15 Linear 0.1}
> in audition (out 0 (atari2600 2 3 10 10 x y 1))

> let {x = mouseX' KR 0 15 Linear 0.1
>     ;o1 = sinOsc KR 0.35 0 * 7.5 + 7.5
>     ;y = mouseY' KR 0 31 Linear 0.1
>     ;o2 = sinOsc KR 0.3 0 * 5.5 + 5.5}
> in audition (out 0 (atari2600 x o1 10 y o2 5 1))

> let ati = let {gate' = control KR "gate" 1
>               ;tone0 = control KR "tone0" 5
>               ;tone1 = control KR "tone1" 8
>               ;freq0 = control KR "freq0" 10
>               ;freq1 = control KR "freq1" 20
>               ;rate = control KR "rate" 1
>               ;amp = control KR "amp" 1
>               ;pan = control KR "pan" 0
>               ;e = envASR 0.01 amp 0.05 (EnvNum (-4))
>               ;eg = envGen KR gate' 1 0 1 RemoveSynth e
>               ;z = atari2600 tone0 tone1 freq0 freq1 15 15 rate
>               ;o = out 0 (pan2 (z * eg) pan 1)}
>           in synthdef "atari2600" o

> import Sound.SC3.Lang.Pattern.ID

> let p = [("dur",0.125)
>         ,("amp",0.5)
>         ,("tone0",pseq [pn 3 64,pn 2 128,pn 10 8] inf)
>         ,("tone1",pseqn [32,12] [8,pwhite 'a' 0 15 inf] inf)
>         ,("freq0",pseqn [17,4,3] [10,prand 'a' [1,2,3] inf,10] inf)
>         ,("freq1",pseq1 [10,3,pwrand 'c' [20,1] [0.6,0.4] inf] inf)]
> in audition (ati,pbind p)

> let p = [("dur",pseq [0.25,0.25,0.25,0.45] inf)
>         ,("amp",0.5)
>         ,("tone0",pseq [pseq [2,5] 32,pseq [3,5] 32] inf)
>         ,("tone1",14)
>         ,("freq0",pseq [pbrown 'a' 28 31 1 32,pbrown 'b' 23 26 3 32] inf)
>         ,("freq1",pseq [pn 10 16,pn 11 16] inf)]
> in audition (ati,pbind p)

> let p = [("dur",pbrown 'a' 0.1 0.15 0.1 inf)
>         ,("amp",0.5)
>         ,("tone0",1)
>         ,("tone1",2)
>         ,("freq0",pseqn [2,1] [24,pwrand 'b' [20,23] [0.6,0.4] inf] inf)
>         ,("freq1",pseqn [1,1,1] [1,3,pwrand 'c' [2,1] [0.6,0.4] inf] inf)]
> in audition (ati,pbind p)
