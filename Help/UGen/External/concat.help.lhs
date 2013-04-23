> Sound.SC3.UGen.Help.viewSC3Help "Concat"
> Sound.SC3.UGen.DB.ugenSummary "Concat"

> import Sound.SC3.ID

> let fileName = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (async (b_allocRead 12 fileName 0 0))

Granulator
> let {y0 = mouseY KR 0.01 1 Linear 0.2
>     ;y1 = mouseY KR 12 100 Linear 0.2
>     ;n = lfNoise0 'α' KR y0 * 3 + 4.5
>     ;k = saw AR (sinOsc KR n 0 * 10 + y1)
>     ;i = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop DoNothing
>     ;x0 = mouseX KR 0.01 0.1 Linear 0.2
>     ;y2 = mouseY KR 0 0.1 Linear 0.2
>     ;c :: UGen
>     ;c = concat' k i 2 2 2 x0 0 y2 1 0.5 0 0
>     ;o = out 0 (pan2 c 0 1)}
> in audition o
