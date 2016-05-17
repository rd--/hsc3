> import Sound.SC3 {- hsc3 -}

Graph with the three types of non-audio controls.

> g_01 =
>     let freq = control KR "freq" 440
>         phase = control IR "phase" 0
>         gate' = tr_control "gate" 1
>         amp = control KR "amp" 0.1
>         e = envGen KR gate' amp 0 1 DoNothing (envASR 0.01 0.1 1 EnvLin)
>     in sinOsc AR freq phase * e

Make a graph drawing

    > import Sound.SC3.UGen.Dot {- hsc3-ugen -}
    > draw (out 0 g_01)

Listen

    > audition_at (10,AddToHead,1,[]) (out 0 g_01)

Set frequency and the trigger gate.

    > withSC3 (send (n_set1 10 "freq" 2200))
    > withSC3 (send (n_set1 10 "gate" 1))

Make a control rate graph to write freq and gate values.

> g_02 = mce2 (tRand 'α' 220 2200 (dust 'β' KR 1)) (dust 'γ' KR 3)

Add it _before_ the node it will map to, the trigger is only on the bus for the current cycle.

    > audition_at (-1,AddBefore,10,[]) (out 0 g_02)

Map the control values at the audio graph.

    > withSC3 (send (n_map 10 [("freq",0),("gate",1)]))
