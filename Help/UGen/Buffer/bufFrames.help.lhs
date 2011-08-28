> Sound.SC3.UGen.Help.viewSC3Help "BufFrames"
> Sound.SC3.UGen.DB.ugenSummary "BufFrames"

> import Sound.SC3

Load sound file to buffer zero (required for examples)
> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (\fd -> async fd (b_allocRead 0 fn 0 0))

Read without loop, trigger reset based on buffer duration
> let p = phasor AR 0 (bufRateScale KR 0) 0 (bufFrames KR 0) 0
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))

Mouse location drags play head
> let {r = mce [0.05,0.075 .. 0.15]
>     ;p = k2A (mouseX' KR 0 (bufFrames KR 0) Linear r)}
> in audition (out 0 (mix (bufRdL 1 AR 0 p NoLoop)))
