> Sound.SC3.UGen.Help.viewSC3Help "BufDur"
> Sound.SC3.UGen.DB.ugenSummary "BufDur"

> import Sound.SC3

Load sound file to buffer zero (required for examples)

> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (async (b_allocRead 0 fn 0 0))

Read without loop, trigger reset based on buffer duration

> let {t = impulse AR (recip (bufDur KR 0)) 0
>     ;p = sweep t (bufSampleRate KR 0)}
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))
