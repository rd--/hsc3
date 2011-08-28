> Sound.SC3.UGen.Help.viewSC3Help "BufRd"
> Sound.SC3.UGen.DB.ugenSummary "BufRd"

> import Sound.SC3.ID

Load sound file to buffer zero (required for examples)
> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (\fd -> async fd (b_allocRead 0 fn 0 0))

Audio rate sine oscillator as phase input
> let phase = (sinOsc AR 0.1 0 * bufFrames KR 0)
> in audition (out 0 (bufRd 1 AR 0 phase Loop NoInterpolation))

There are constructors, bufRd{N|L|C}, for the fixed cases.
> let {x = mouseX' KR (mce [5, 10]) 100 Linear 0.1
>     ;n = lfNoise1 'a' AR x}
> in audition (out 0 (bufRdL 1 AR 0 (n * bufFrames KR 0) Loop))
