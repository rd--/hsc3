    > Sound.SC3.UGen.Help.viewSC3Help "FBSineC"
    > Sound.SC3.UGen.DB.ugenSummary "FBSineC"

> import Sound.SC3 {- hsc3 -}

SC3 default values.

> g_01 = fbSineC AR (sampleRate / 4) 1 0.1 1.1 0.5 0.1 0.1 * 0.2

Increase feedback

> g_02 =
>     let fb = line KR 0.01 4 10 DoNothing
>     in fbSineC AR sampleRate 1 fb 1.1 0.5 0.1 0.1 * 0.2

Increase phase multiplier

> g_03 =
>     let a = line KR 1 2 10 DoNothing
>     in fbSineC AR sampleRate 1 0 a 0.5 0.1 0.1 * 0.2

Randomly modulate parameters

> g_04 =
>     let madd a m = (+ a) . (* m)
>         x = mouseX KR 1 12 Linear 0.1
>         n e = lfNoise2 e KR x
>         n0 = madd 1e4 1e4 (n 'α')
>         n1 = madd 33 32 (n 'β')
>         n2 = madd 0 0.5 (n 'γ')
>         n3 = madd 1.05 0.05 (n 'δ')
>         n4 = madd 0.3 0.3 (n 'ε')
>     in fbSineC AR n0 n1 n2 n3 n4 0.1 0.1 * 0.2
