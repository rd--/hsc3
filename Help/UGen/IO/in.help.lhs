in' numChannels rate bus

Read signal from an audio or control bus.
 
Patching input to output.

> audition $ out 0 (in' 2 AR numInputBuses)

Patching input to output, with delay.

> let i = in' 2 AR numInputBuses
>     d = delayN i 0.5 0.5
> audition $ out 0 (i + d)

Write noise to bus 10, then read it out.  The MRG is ordered.

> n <- pinkNoise AR
> let wr = out 10 (n * 0.3)
>     rd = out 0 (in' 1 AR 10)
> audition $ MRG [rd, wr]

Reading a control bus.

> withSC3 (\fd -> send fd (c_set 0 300))

> audition $ sinOsc AR (in' 1 KR 0) 0 * 0.1

> withSC3 (\fd -> send fd (c_set 0 600))
