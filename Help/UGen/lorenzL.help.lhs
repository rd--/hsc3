    Sound.SC3.UGen.Help.viewSC3Help "LorenzL"
    Sound.SC3.UGen.DB.ugenSummary "LorenzL"

> import Sound.SC3 {- hsc3 -}

Vary frequency

> g_01 =
>     let x = mouseX KR 20 sampleRate Linear 0.1
>     in lorenzL AR x 10 27 2.667 0.05 0.1 0 0 * 0.3

Randomly modulate params

> g_02 =
>     let n e = lfNoise0 e KR 0.5
>         n0 = mul_add (n 'α') 2 10
>         n1 = mul_add (n 'β') 20 38
>         n2 = mul_add (n 'γ') 1.5 2
>     in lorenzL AR sampleRate n0 n1 n2 0.05 0.1 0 0 * 0.2

As frequency control

> g_03 =
>     let x = mouseX KR 1 200 Linear 0.1
>         n = lorenzL AR x 10 28 2.667 0.05 0.1 0 0
>     in sinOsc AR (lag n 0.003 * 800 + 900) 0 * 0.4

> g_04 =
>     let x = mouseX KR 1 200 Linear 0.1
>         n = lorenzL AR x 10 28 2.667 0.05 0.1 0 0
>     in impulse AR (n * 4 + 8) 0 * 0.4
