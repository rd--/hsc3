    > Sound.SC3.UGen.Help.viewSC3Help "BufRateScale"
    > Sound.SC3.UGen.DB.ugenSummary "BufRateScale"

> import Sound.SC3 {- hsc 3 -}

Load sound file to buffer zero (required for examples)

> fn_01 = "/home/rohan/data/audio/pf-c5.aif"

    > withSC3 (async (b_allocRead 0 fn_01 0 0))

Read buffer at 3/4 reported sample rate.

> g_01 =
>     let r = 0.75 * bufRateScale KR 0
>         p = phasor AR 0 r 0 (bufFrames KR 0) 0
>     in bufRdL 1 AR 0 p NoLoop
