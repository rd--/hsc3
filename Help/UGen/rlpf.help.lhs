    Sound.SC3.UGen.Help.viewSC3Help "RLPF"
    Sound.SC3.UGen.DB.ugenSummary "RLPF"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let n = whiteNoise 'α' AR
>       f = sinOsc AR 0.5 0 * 40 + 220
>   in rlpf n f 0.1

> g_02 =
>   let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
>   in rlpf (saw AR 200 * 0.1) f 0.2

> g_03 =
>   let ctl = rlpf (saw AR 5 * 0.1) 25 0.03
>   in sinOsc AR (ctl * 200 + 400) 0 * 0.1

> g_04 =
>   let x = mouseX KR 2 200 Exponential 0.2
>       y = mouseY KR 0.01 1 Exponential 0.2
>       ctl = rlpf (saw AR 5 * 0.1) x y
>   in sinOsc AR (ctl * 200 + 400) 0 * 0.1
