> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let f = linRand 'α' 200.0 10000.0 (mce [-1, 1])
>       e = line KR 0.4 0 0.01 RemoveSynth
>   in fSinOsc AR f 0 * e
