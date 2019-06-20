    :t selectX

selectX is a composite ugen graph

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let n = 3/2
>       f = mce2 440 441
>       a = mce [sinOsc AR f 0, saw AR f, pulse AR f 0.1]
>   in mix (selectX (lfSaw KR 1 0 * n + n) a * 0.2)

Here used as a sequencer:

> g_02 =
>   let n = 10
>       a = mce [517, 403, 89, 562, 816, 107, 241, 145, 90, 224]
>       c = n / 2
>       f = mix (selectX (lfSaw KR 0.5 0 * c + c) a)
>   in saw AR f * 0.2
