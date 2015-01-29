> Sound.SC3.UGen.Help.viewSC3Help "HenonN"
> Sound.SC3.UGen.DB.ugenSummary "HenonN"

> import Sound.SC3

With SC3 default initial parameters.

> let x = mouseX KR 20 sampleRate Linear 0.1
> in audition (out 0 (henonN AR x 1.4 0.3 0 0 * 0.1))

With mouse-control of parameters.

> let {x = mouseX KR 1 1.4 Linear 0.1
>     ;y = mouseY KR 0 0.3 Linear 0.1}
> in audition (out 0 (henonN AR (sampleRate / 4) x y 0 0 * 0.1))

With randomly modulated parameters.

> let {n0 = lfNoise2 'a' KR 1 * 0.20 + 1.20
>     ;n1 = lfNoise2 'a' KR 1 * 0.15 + 0.15}
> in audition (out 0 (henonN AR (sampleRate / 8) n0 n1 0 0 * 0.1))

As a frequency control.

> let {x = mouseX KR 1 1.4 Linear 0.1
>     ;y = mouseY KR 0 0.3 Linear 0.1
>     ;f0 = 40
>     ;f = henonN AR f0 x y 0 0 * 800 + 900}
> in audition (out 0 (sinOsc AR f 0 * 0.4))
