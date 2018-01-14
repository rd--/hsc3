    Sound.SC3.UGen.Help.viewSC3Help "LFNoise1"
    Sound.SC3.UGen.DB.ugenSummary "LFNoise1"

> import Sound.SC3 {- hsc3 -}

> g_01 = lfNoise1 'α' AR 1000 * 0.05

Modulate frequency.

> g_02 =
>   let f = xLine KR 1000 10000 10 RemoveSynth
>   in lfNoise1 'α' AR f * 0.05

Use as frequency control.

> g_03 =
>   let n = lfNoise1 'α' KR 4
>       f = n * 400 + 450
>   in sinOsc AR f 0 * 0.1
