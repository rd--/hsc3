> import Sound.SC3 {- hsc3 -}

for fast x lfClipNoise frequently seems stuck, lfdClipNoise changes smoothly

> g_01 =
>   let x = mouseX KR 0.1 1000 Exponential 0.2
>       n = lfdClipNoise 'α' AR x
>   in sinOsc AR (n * 200 + 500) 0 * 0.05

> g_02 =
>   let x = mouseX KR 0.1 1000 Exponential 0.2
>       n = lfClipNoise 'β' AR x
>   in sinOsc AR (n * 200 + 500) 0 * 0.05

lfClipNoise quantizes time steps at high freqs, lfdClipNoise does not:

> g_03 =
>   let f = xLine KR 1000 20000 10 RemoveSynth
>   in lfdClipNoise 'γ' AR f * 0.05

> g_04 =
>   let f = xLine KR 1000 20000 10 RemoveSynth
>   in lfClipNoise 'δ' AR f * 0.05
