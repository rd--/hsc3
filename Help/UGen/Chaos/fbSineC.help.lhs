> Sound.SC3.UGen.Help.viewSC3Help "FBSineC"
> Sound.SC3.UGen.DB.ugenSummary "FBSineC"

> import Sound.SC3.ID

SC3 default values.

> let o = fbSineC AR (sampleRate / 4) 1 0.1 1.1 0.5 0.1 0.1 * 0.2
> in audition (out 0 o)

Increase feedback

> let {fb = line KR 0.01 4 10 DoNothing
>     ;o = fbSineC AR sampleRate 1 fb 1.1 0.5 0.1 0.1 * 0.2}
> in audition (out 0 o)

Increase phase multiplier

> let {a = line KR 1 2 10 DoNothing
>     ;o = fbSineC AR sampleRate 1 0 a 0.5 0.1 0.1 * 0.2}
> in audition (out 0 o)

Randomly modulate parameters

> let {madd a m = (+ a) . (* m)
>     ;x = mouseX' KR 1 12 Linear 0.1
>     ;n e = lfNoise2 e KR x
>     ;n0 = madd 1e4 1e4 (n 'a')
>     ;n1 = madd 33 32 (n 'b')
>     ;n2 = madd 0 0.5 (n 'c')
>     ;n3 = madd 1.05 0.05 (n 'd')
>     ;n4 = madd 0.3 0.3 (n 'e')}
> in audition (out 0 (fbSineC AR n0 n1 n2 n3 n4 0.1 0.1 * 0.2))
