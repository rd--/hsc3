> Sound.SC3.UGen.Help.viewSC3Help "Select"
> Sound.SC3.UGen.DB.ugenSummary "Select"

> import Sound.SC3

> let { n = 3/2
>     ; a = mce [sinOsc AR 440 0, saw AR 440, pulse AR 440 0.1] }
> in audition (out 0 (select (lfSaw KR 1 0 * n + n) a * 0.2))

Here used as a sequencer:
> let { n = 10
>     ; a = mce [517, 403, 89, 562, 816, 107, 241, 145, 90, 224]
>     ; c = n / 2
>     ; f = select (lfSaw KR 0.5 0 * c + c) a }
> in audition (out 0 (saw AR f * 0.2))

