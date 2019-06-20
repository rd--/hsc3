> import Sound.SC3

Load sound file to buffer zero (required for examples)

> fn_01 = "/home/rohan/data/audio/pf-c5.aif"


> m_01 = b_allocRead 0 fn_01 0 0

    > withSC3 (async m_01)

Read without loop, trigger reset based on buffer duration

> g_01 =
>     let p = phasor AR 0 (bufRateScale KR 0) 0 (bufFrames KR 0) 0
>     in bufRdL 1 AR 0 p NoLoop

Mouse location drags play head

> g_02 =
>     let r = mce [0.05,0.075 .. 0.15]
>         p = k2a (mouseX KR 0 (bufFrames KR 0) Linear r)
>     in mix (bufRdL 1 AR 0 p NoLoop)
