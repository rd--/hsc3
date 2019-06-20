> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let n = 3/2
>         a = mce [sinOsc AR 440 0, saw AR 440, pulse AR 440 0.1]
>     in select (lfSaw KR 1 0 * n + n) a * 0.2

Here used as a sequencer:

> g_02 =
>     let n = 10
>         a = mce [517, 403, 89, 562, 816, 107, 241, 145, 90, 224]
>         c = n / 2
>         f = select (lfSaw KR 0.5 0 * c + c) a
>     in saw AR f * 0.2

i-rate...

> g_03 =
>     let a = mce [rand 'α' 110 220,rand 'β' 220 440,rand 'γ' 440 880]
>     in sinOsc AR (select (rand 'δ' 0 3) a) 0 * 0.1
