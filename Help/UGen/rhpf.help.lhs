> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
>   in rhpf (saw AR 200 * 0.1) f 0.2
