    > Sound.SC3.UGen.Help.viewSC3Help "BufFrames"
    > Sound.SC3.UGen.DB.ugenSummary "BufFrames"

> import Sound.SC3

Load sound file to buffer zero (required for examples)

> fn_01 = "/home/rohan/data/audio/pf-c5.aif"

    > withSC3 (async (b_allocRead 0 fn_01 0 0))

Read without loop, trigger reset based on buffer duration

> g_01 =
>     let p = phasor AR 0 (bufRateScale KR 0) 0 (bufFrames KR 0) 0
>     in bufRdL 1 AR 0 p NoLoop

Mouse location drags play head

> g_02 =
>     let r = mce [0.05,0.075 .. 0.15]
>         p = k2A (mouseX KR 0 (bufFrames KR 0) Linear r)
>     in mix (bufRdL 1 AR 0 p NoLoop)
