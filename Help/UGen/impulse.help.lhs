Note: SC2 had no phase input.

> import Sound.SC3 {- hsc3 -}

> g_01 = impulse AR 800 0 * 0.1

> g_02 =
>     let f = xLine KR 800 10 5 RemoveSynth
>     in impulse AR f 0.0 * 0.1

> g_03 =
>     let f = mouseY KR 4 8 Linear 0.1
>         x = mouseX KR 0 1 Linear 0.1
>     in impulse AR f (mce [0,x]) * 0.1

An impulse with frequency 0 returns a single impulse

> g_04 = decay (impulse AR 0 0) 1 * brownNoise 'α' AR * 0.1

Non-bandlimited

> g_05 =
>   let f = sinOsc AR 0.25 0.0 * 2500.0 + 2505.0
>   in impulse AR f 0.0 * 0.1

Drawings:

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen_nrt (48000,64) 1.0 (impulse AR 10 0)
