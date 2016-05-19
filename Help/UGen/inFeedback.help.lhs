    > Sound.SC3.UGen.Help.viewSC3Help "InFeedback"
    > Sound.SC3.UGen.DB.ugenSummary "InFeedback"

> import Sound.SC3 {- hsc3 -}

Audio feedback modulation

> g_01 =
>     let f = inFeedback 1 0 * 1300 + 300
>     in sinOsc AR f 0 * 0.4

Audition these in either order and hear both tones.

> g_02 = inFeedback 1 firstPrivateBus
>
> g_03 =
>     let b  = firstPrivateBus
>         s0 = out b (sinOsc AR 220 0 * 0.1)
>         s1 = out 0 (sinOsc AR 660 0 * 0.1)
>     in mrg [s0, s1]

Doubters consult this

> g_04 = let b = firstPrivateBus in in' 1 AR b

Resonator, see localOut for variant.

> g_05 =
>     let b = firstPrivateBus
>         p = inFeedback 1 b
>         i = impulse AR 1 0
>         d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)
>     in mrg [offsetOut b d, offsetOut 0 p]

Compare with oscillator (at right).

> g_06 = out 1 (sinOsc AR 440 0 * 0.2)
