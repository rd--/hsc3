    Sound.SC3.UGen.Help.viewSC3Help "BufDur"
    Sound.SC3.UGen.DB.ugenSummary "BufDur"

> import Sound.SC3 {- hsc3 -}

Load sound file to buffer zero (required for examples)

> fn_01 = "/home/rohan/data/audio/pf-c5.aif"

    > withSC3 (async (b_allocRead 0 fn_01 0 0))

Read without loop, trigger reset based on buffer duration

> g_01 =
>     let t = impulse AR (recip (bufDur KR 0)) 0
>         p = sweep t (bufSampleRate KR 0)
>     in bufRdL 1 AR 0 p NoLoop
