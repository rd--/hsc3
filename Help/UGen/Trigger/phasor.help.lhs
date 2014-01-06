> Sound.SC3.UGen.Help.viewSC3Help "Phasor"
> Sound.SC3.UGen.DB.ugenSummary "Phasor"

> import Sound.SC3

phasor controls sine frequency, end frequency matches second sine.

> let {rate = mouseX KR 0.2 2 Exponential 0.1
>     ;tr = impulse AR rate 0
>     ;sr = sampleRate
>     ;x = phasor AR tr (rate / sr) 0 1 0
>     ;f = mce [linLin x 0 1 600 1000, 1000]}
> in audition (out 0 (sinOsc AR f 0 * 0.2))

Load sound file to buffer zero

> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (async (b_allocRead 0 fn 0 0))

Phasor as phase input to bufRd

> let ph = phasor AR 0 (bufRateScale KR 0) 0 (bufFrames KR 0) 0
> in audition (out 0 (bufRdN 1 AR 0 ph Loop))

Allocate and generate (non-wavetable) buffer at index one
(see osc for wavetable oscillator)

> withSC3 (do {_ <- async (b_alloc 1 256 1)
>             ;let f = [Normalise,Clear]
>              in send (b_gen_sine1 1 f [1])})

Audio rate phasor oscillator as phase input to bufRd

> let {b = 1
>     ;f = 440
>     ;fr = bufFrames KR b
>     ;rt = f * (fr / sampleRate)
>     ;ph = phasor AR b (rt * bufRateScale KR b) 0 fr 0}
> in audition (out 0 (bufRdL 1 AR b ph Loop * 0.1))
