    > Sound.SC3.UGen.Help.viewSC3Help "Onsets"
    > Sound.SC3.UGen.DB.ugenSummary "Onsets"

> import Sound.SC3 {- hsc3 -}

    > withSC3 (async (b_alloc 10 512 1))

> f_01 t = t2A t 0

> f_02 t =
>     let s = sinOsc AR 440 0 * 0.2
>         e = envGen KR t 1 0 1 DoNothing (envPerc 0.001 0.1)
>     in s * e

> f_03 f =
>     let x = mouseX KR 0 1 Linear 0.2
>         i = mix (soundIn (mce2 0 1))
>         c = fft' 10 i
>         o = onsets' c x (onsetType "rcomplex")
>     in f o

> g_01 = f_03 f_01

> g_02 = mce2 (dc AR 0) (mix (soundIn (mce2 0 1) * 0.75))

a generative signal with distinct onsets!

> g_03 =
>     let e = linLin (saw AR 2) (-1) 1 0 1
>         p = let f = midiCPS (tIRand 'α' 63 75 (impulse KR 2 0))
>             in pulse AR f 0.5
>         f = linExp (lfNoise2 'β' KR 0.5) (-1) 1 100 10000
>     in lpf p f * e

x varies threshold, whitenoise bursts indicate detected onsets

> g_04 =
>     let c = fft' 10 g_03
>         x = mouseX KR 0 1 Linear 0.2
>         o = onsets' c x (onsetType "rcomplex")
>         p = let d = envPerc 0.001 0.1
>             in whiteNoise 'α' AR * envGen KR o 0.2 0 1 DoNothing d
>     in pan2 g_03 (-0.75) 0.2 + pan2 p 0.75 1
