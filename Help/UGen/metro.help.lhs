> Sound.SC3.UGen.Help.viewSC3Help "Metro"
> Sound.SC3.UGen.DB.ugenSummary "Metro"

> import Sound.SC3

> audition (out 0 (metro AR 60 1))

> let {b = xLine KR 60 120 5 DoNothing
>     ;m = metro KR b 1
>     ;o = sinOsc AR 440 0 * 0.1}
> in audition (out 0 (decay m 0.2 * o))

> let {b = range 30 240 (lfNoise2 'α' KR 0.2)
>     ;n = dseq 'β' dinf (mce [1,0.25,0.5,0.25])
>     ;a = decay (metro KR b n) 0.2 * sinOsc AR 440 0 * 0.1}
> in audition (out 0 a)
