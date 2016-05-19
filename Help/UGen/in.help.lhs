    > Sound.SC3.UGen.Help.viewSC3Help "In"
    > Sound.SC3.UGen.DB.ugenSummary "In"

Note: `hsc3` renames UGen to `in'` since `in` is a reserved keyword

> import Sound.SC3 {- hsc3 -}

Patching input to output (see also soundIn).

> g_01 = in' 2 AR numOutputBuses

Patching input to output, with delay.

> g_02 =
>     let i = in' 2 AR numOutputBuses
>         d = delayN i 0.5 0.5
>     in i + d

Write noise to first private bus, then read it out.
The multiple root graph is ordered.

> g_03 =
>     let n = pinkNoise 'α' AR
>         b = numOutputBuses + numInputBuses
>         wr = out b (n * 0.3)
>         rd = out 0 (in' 1 AR b)
>     in mrg [rd,wr]

There are functions to encapsulate the offset calculation.
(There is also a firstPrivateBus value.)

> g_04 =
>     let n = pinkNoise 'α' AR
>         wr = privateOut 0 (n * 0.3)
>         rd = out 0 (privateIn 1 AR 0)
>     in mrg [rd,wr]

Set value on a control bus

    > withSC3 (send (c_set1 0 300))

Read a control bus

> g_05 = sinOsc AR (in' 1 KR 0) 0 * 0.1

Re-set value on bus

    > withSC3 (send (c_set1 0 600))

Control rate graph writing buses 0 & 1.

> g_06 = mce2 (tRand 'α' 220 2200 (dust 'β' KR 1)) (dust 'γ' KR 3)

Audio rate graph reading control buses 0 & 1.

> g_07 = sinOsc AR (in' 1 KR 0) 0 * decay (in' 1 KR 1) 0.2 * 0.1
