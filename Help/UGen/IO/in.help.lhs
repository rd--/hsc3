in' numChannels rate bus

Read signal from an audio or control bus.

Patching input to output.

> import Sound.SC3

> audition (out 0 (in' 2 AR numOutputBuses))

Patching input to output, with delay.

> let { i = in' 2 AR numOutputBuses
>     ; d = delayN i 0.5 0.5 }
> in audition (out 0 (i + d))

Write noise to bus 10, then read it out.  The multiple root graph is ordered.

> import Sound.SC3.ID

> let { n = pinkNoise 'a' AR
>     ; wr = out 10 (n * 0.3)
>     ; rd = out 0 (in' 1 AR 10) }
> in audition (mrg [rd, wr])

Reading a control bus.

> withSC3 (\fd -> send fd (c_set [(0, 300)]))

> audition (out 0 (sinOsc AR (in' 1 KR 0) 0 * 0.1))

> withSC3 (\fd -> send fd (c_set [(0, 600)]))
