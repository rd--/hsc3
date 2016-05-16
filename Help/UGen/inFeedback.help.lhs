> Sound.SC3.UGen.Help.viewSC3Help "InFeedback"
> Sound.SC3.UGen.DB.ugenSummary "InFeedback"

> import Sound.SC3

Audio feedback modulation

> let {f = inFeedback 1 0 * 1300 + 300
>     ;s = sinOsc AR f 0 * 0.4}
> in audition (out 0 s)

Evaluate these in either order and hear both tones.

> let {b = firstPrivateBus
>     ;s = inFeedback 1 b}
> in audition (out 0 s)

> let {b  = firstPrivateBus
>     ;s0 = out b (sinOsc AR 220 0 * 0.1)
>     ;s1 = out 0 (sinOsc AR 660 0 * 0.1)}
> in audition (mrg [s0, s1])

Doubters consult this

> let {b = firstPrivateBus
>     ;s = in' 1 AR b}
> in audition (out 0 s)

Resonator, see localOut for variant.

> let {b = firstPrivateBus
>     ;p = inFeedback 1 b
>     ;i = impulse AR 1 0
>     ;d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)}
> in audition (mrg [offsetOut b d, offsetOut 0 p])

Compare with oscillator.

> audition (out 1 (sinOsc AR 440 0 * 0.2))
