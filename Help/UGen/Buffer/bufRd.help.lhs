> Sound.SC3.UGen.Help.viewSC3Help "BufRd"
> Sound.SC3.UGen.DB.ugenSummary "BufRd"

> import Sound.SC3.ID {- hsc3 -}

Load sound file to buffer zero (required for examples)

> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (async (b_allocRead 0 fn 0 0))

Phasor as phase input

> let {sc = bufRateScale KR 0
>     ;tr = impulse AR (recip (bufDur KR 0)) 0
>     ;ph = phasor AR tr sc 0 (bufFrames KR 0) 0}
> in audition (out 0 (bufRdL 1 AR 0 ph NoLoop))

Audio rate sine oscillator as phase input

> let phase = sinOsc AR 0.1 0 * bufFrames KR 0 * bufRateScale KR 0
> in audition (out 0 (bufRd 1 AR 0 phase Loop NoInterpolation))

There are constructors, bufRd{N|L|C}, for the fixed cases.

> let {x = mouseX KR (mce [5, 10]) 100 Linear 0.1
>     ;n = lfNoise1 'α' AR x}
> in audition (out 0 (bufRdL 1 AR 0 (n * bufFrames KR 0 * bufRateScale KR 0) Loop))

Allocate and generate (non-wavetable) buffer

> withSC3 (do {_ <- async (b_alloc 11 256 1)
>             ;let f = [Normalise,Clear]
>              in send (b_gen_sine1 11 f [1,1/2,1/3,1/4,1/5])})

Fixed frequency wavetable oscillator

> let phase = linLin (saw AR 440) (-1) 1 0 1 * bufFrames KR 11
> in audition (out 0 (bufRd 1 AR 11 phase Loop NoInterpolation * 0.1))
