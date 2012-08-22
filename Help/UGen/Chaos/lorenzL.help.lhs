> Sound.SC3.UGen.Help.viewSC3Help "LorenzL"
> Sound.SC3.UGen.DB.ugenSummary "LorenzL"

> import Sound.SC3.ID

Vary frequency
> let x = mouseX KR 20 sampleRate Linear 0.1
> in audition (out 0 (lorenzL AR x 10 27 2.667 0.05 0.1 0 0 * 0.3))

Randomly modulate params
> let {madd a m = (+ a) . (* m)
>     ;n e = lfNoise0 e KR 0.5
>     ;n0 = madd 10 2 (n 'a')
>     ;n1 = madd 38 20 (n 'b')
>     ;n2 = madd 2 1.5 (n 'c')
>     ;o = lorenzL AR sampleRate n0 n1 n2 0.05 0.1 0 0 * 0.2}
> in audition (out 0 o)

As frequency control
> let {x = mouseX KR 1 200 Linear 0.1
>     ;n = lorenzL AR x 10 28 2.667 0.05 0.1 0 0}
> in audition (out 0 (sinOsc AR (lag n 0.003 * 800 + 900) 0 * 0.4))
