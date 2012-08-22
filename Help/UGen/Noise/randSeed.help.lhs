> Sound.SC3.UGen.Help.viewSC3Help "RandSeed"
> Sound.SC3.UGen.DB.ugenSummary "RandSeed"

> import Sound.SC3.ID

start a noise patch
> let {n = udup 2 (whiteNoise 'a' AR * 0.05 + dust2 'a' AR 70)
>     ;f = lfNoise1 'a' KR 3 * 5500 + 6000
>     ;r = resonz (n * 5) f 0.5 + n * 0.5}
> in audition (out 0 r)

reset the seed at a variable rate
> let {s = control KR "seed" 1956
>     ;r = randSeed KR (impulse KR (mouseX KR 0.1 100 Linear 0.2) 0) s}
> in audition r

always the same (for a given seed)...
> let {sd = 1957
>     ;n = tIRand 'a' 4 12 (dust 'a' KR 1)
>     ;f = n * 150 + (mce [0,1])
>     ;r = randSeed IR 1 sd}
> in audition (out 0 (mrg2 (sinOsc AR f 0 * 0.1) r))
