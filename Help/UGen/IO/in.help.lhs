> Sound.SC3.UGen.Help.viewSC3Help "In"
> Sound.SC3.UGen.DB.ugenSummary "In"

# hsc3 renames UGen to in' since in is a reserved keyword

> import Sound.SC3.ID

Patching input to output.
> audition (out 0 (in' 2 AR numOutputBuses))

Patching input to output, with delay.
> let {i = in' 2 AR numOutputBuses
>     ;d = delayN i 0.5 0.5}
> in audition (out 0 (i + d))

Write noise to bus 10, then read it out, the multiple root graph is ordered.
> let {n = pinkNoise 'a' AR
>     ;wr = out 10 (n * 0.3)
>     ;rd = out 0 (in' 1 AR 10)}
> in audition (mrg [rd, wr])

Set value on a control bus
> withSC3 (\fd -> send fd (c_set [(0, 300)]))

Read a control bus
> audition (out 0 (sinOsc AR (in' 1 KR 0) 0 * 0.1))

Re-set value on bus
> withSC3 (\fd -> send fd (c_set [(0, 600)]))
