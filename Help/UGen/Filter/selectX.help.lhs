> Sound.SC3.UGen.Help.viewSC3Help "SelectX"
> :t selectX

# composite ugen graph

> import Sound.SC3
> import Sound.SC3.UGen.Dot

> let { n = 3/2
>     ; f = mce2 440 441
>     ; a = mce [sinOsc AR f 0, saw AR f, pulse AR f 0.1]
>     ; s = mceSum (selectX (lfSaw KR 1 0 * n + n) a * 0.2) }
> in audition (out 0 s) >> draw s

Here used as a sequencer:
> let { n = 10
>     ; a = mce [517, 403, 89, 562, 816, 107, 241, 145, 90, 224]
>     ; c = n / 2
>     ; f = mceSum (selectX (lfSaw KR 0.5 0 * c + c) a)
>     ; s = saw AR f * 0.2 }
> in audition (out 0 s) >> draw s
