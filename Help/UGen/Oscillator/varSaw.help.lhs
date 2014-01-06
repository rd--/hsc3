> Sound.SC3.UGen.Help.viewSC3Help "VarSaw"
> Sound.SC3.UGen.DB.ugenSummary "VarSaw"

> import Sound.SC3

> let {f = lfPulse KR (mce2 3 3.03) 0 0.3 * 200 + 200
>     ;w = linLin (lfTri KR 1 0) (-1) 1 0 1}
> in audition (out 0 (varSaw AR f 0 w * 0.1))

Compare with lfPulse at AR

> let f = lfPulse KR 3 0 0.3 * 200 + 200
> in audition (out 0 (mce [varSaw AR f 0 0.2
>                         ,lfPulse AR f 0 0.2] * 0.1))

per-note width modulation

> let {d = linLin (lfNoise2 'α' KR 0.1) (-1) 1 0.15 0.5
>     ;t = impulse AR (1 / d) 0
>     ;w0 = tRand 'β' 0 0.35 t
>     ;w1 = tRand 'γ' 0.65 1 t
>     ;w = phasor AR t ((w1 - w0) * sampleDur) w0 w1 0
>     ;e = decay2 t 0.1 d
>     ;f = midiCPS (tRand 'δ' 36 72 t)
>     ;o = varSaw AR f 0 w * e * 0.1
>     ;l = tRand 'ε' (-1) 1 t}
> in audition (out 0 (pan2 o l 1))
