> Sound.SC3.UGen.Help.viewSC3Help "BufRateScale"
> Sound.SC3.UGen.DB.ugenSummary "BufRateScale"

> import Sound.SC3.ID

Load sound file to buffer zero (required for examples)
> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (\fd -> async fd (b_allocRead 0 fn 0 0))

Read buffer at 3/4 reported sample rate.
> let {r = 0.75 * bufRateScale KR 0
>     ;p = phasor AR 0 r 0 (bufFrames KR 0) 0}
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))
