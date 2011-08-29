> Sound.SC3.UGen.Help.viewSC3Help "TWindex"
> Sound.SC3.UGen.DB.ugenSummary "TWindex"

> import Sound.SC3.ID

> let {p = mce [1/5, 2/5, 2/5]
>     ;a = mce [400, 500, 600]
>     ;t = impulse KR 6 0
>     ;i = tWindex 'a' t 0 p}
> in audition (out 0 (sinOsc AR (select i  a) 0 * 0.1))

Modulating probability values
> let {p = mce [1/4, 1/2, sinOsc KR 0.3 0 * 0.5 + 0.5]
>     ;a = mce [400, 500, 600]
>     ;t = impulse KR 6 0
>     ;i = tWindex 'a' t 1 p}
> in audition (out 0 (sinOsc AR (select i a) 0 * 0.1))
