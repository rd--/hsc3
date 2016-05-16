    Sound.SC3.UGen.Help.viewSC3Help "AllpassN"
    Sound.SC3.UGen.DB.ugenSummary "AllpassN"

> import Sound.SC3

Since the allpass delay has no audible effect as a resonator on steady
state sound ...

> g_01 =
>     let dly = xLine KR 0.0001 0.01 20 RemoveSynth
>         n = whiteNoise 'α' AR
>     in allpassC (n * 0.1) 0.01 dly 0.2

...these examples add the input to the effected sound so that you
can hear the effect of the phase comb.

> g_02 =
>     let n = whiteNoise 'β' AR
>         dly = xLine KR 0.0001 0.01 20 RemoveSynth
>     in (n + allpassN (n * 0.1) 0.01 dly 0.2) * 0.1

Linear variant

> g_03 =
>     let n = whiteNoise 'γ' AR
>         dly = xLine KR 0.0001 0.01 20 RemoveSynth
>     in (n + allpassL (n * 0.1) 0.01 dly 0.2) * 0.1

Cubic variant

> g_04 =
>     let n = whiteNoise 'δ' AR
>         dly = xLine KR 0.0001 0.01 20 RemoveSynth
>     in (n + allpassC (n * 0.1) 0.01 dly 0.2) * 0.1

Used as an echo - doesn't really sound different than Comb, but it
outputs the input signal immediately (inverted) and the echoes are
lower in amplitude.

> g_05 =
>     let n = whiteNoise 'ε' AR
>         d = dust 'ζ' AR 1
>         src = decay (d * 0.5) 0.2 * n
>     in allpassN src 0.2 0.2 3
