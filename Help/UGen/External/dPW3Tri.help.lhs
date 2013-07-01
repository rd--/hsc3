> Sound.SC3.UGen.Help.viewSC3Help "DPW3Tri"
> Sound.SC3.UGen.DB.ugenSummary "DPW3Tri"

> import Sound.SC3.ID

distortion creeps in under 200Hz
> let o = dPW3Tri AR (xLine KR 2000 20 10 DoNothing)
> in audition (out 0 (o * 0.1))

very fast sweeps can have transient distortion effects
> let o = dPW3Tri AR (mouseX KR 200 12000 Exponential 0.2)
> in audition (out 0 (o * 0.1))

compare
> let o = lfTri AR (mouseX KR 200 12000 Exponential 0.2) 0
> in audition (out 0 (o * 0.1))

(for randN)
> import Sound.SC3.UGen.External.RDU.ID

less efficient than LFTri
> let f = randN 50 'a' 50 5000
> in audition (out 0 (splay (dPW3Tri AR f) 1 0.1 0 True))

> let f = randN 50 'a' 50 5000
> in audition (out 0 (splay (lfTri AR f 0) 1 0.1 0 True))

triangle is integration of square wave
> let {f = mouseX KR 440 8800 Exponential 0.2
>     ;o = pulse AR f 0.5
>     ;s = integrator o 0.99}
> in audition (out 0 (s * 0.05))

differentiation of triangle is square
> let {f = mouseX KR 440 8800 Exponential 0.2
>     ;o = dPW3Tri AR f
>     ;s = hpz1 (o * 2)}
> in audition (out 0 (s * 0.25))

compare
> let f = mouseX KR 440 8800 Exponential 0.2
> in audition (out 0 (pulse AR f 0.5 * 0.1))
