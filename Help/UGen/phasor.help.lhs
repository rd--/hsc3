    Sound.SC3.UGen.Help.viewSC3Help "Phasor"
    Sound.SC3.UGen.DB.ugenSummary "Phasor"

> import Sound.SC3

phasor controls sine frequency, end frequency matches second sine.

> g_00 =
>     let rate = mouseX KR 0.2 2 Exponential 0.1
>         tr = impulse AR rate 0
>         sr = sampleRate
>         x = phasor AR tr (rate / sr) 0 1 0
>         f = mce [linLin x 0 1 600 1000, 1000]
>     in sinOsc AR f 0 * 0.2

two phasors control two sine frequencies: mouse y controls resetPos of the second

> g_01 =
>     let rate = mouseX KR 1 200 Linear 0.1
>         tr = impulse AR rate 0
>         sr = sampleRate
>         x = phasor AR tr (rate / sr) 0 1 (mce2 0 (mouseY KR 0 1 Linear 0.2))
>     in sinOsc AR (x * 500 + 500) 0 * 0.2

Load sound file to buffer zero

    > let fn = "/home/rohan/data/audio/pf-c5.aif"
    > withSC3 (async (b_allocRead 0 fn 0 0))

Phasor as phase input to bufRd

> g_02 =
>      let ph = phasor AR 0 (bufRateScale KR 0) 0 (bufFrames KR 0) 0
>      in bufRdN 1 AR 0 ph Loop

Allocate and generate (non-wavetable) buffer at index one
(see osc for wavetable oscillator)

    > withSC3 (mapM_ maybe_async [b_alloc 1 256 1,b_gen_sine1 1 [Normalise,Clear] [1]])

Audio rate phasor oscillator as phase input to bufRd

> g_03 =
>     let b = 1
>         f = 440
>         fr = bufFrames KR b
>         rt = f * (fr / sampleRate)
>         ph = phasor AR b (rt * bufRateScale KR b) 0 fr 0
>     in bufRdL 1 AR b ph Loop * 0.1

Phasor as impulse with reset

> g_04 =
>     let impulse_reset freq reset =
>             let ph = phasor AR reset (freq / sampleRate) 0 1 0
>             in hpz1 ph <** 0
>         x = mouseX KR 0 1 Linear 0.2 >** 0.5
>         ck = impulse AR 3 0
>         im = impulse_reset 3 x
>         x' = sinOsc AR 440 0 * x * 0.05
>         im' = sinOsc AR 220 0 * decay2 (ck + im) 0.01 0.5 * 0.1
>     in mce2 x' im'

If one wants Phasor to output a signal with frequency freq oscilating
between start and end, then the rate should be (end - start) * freq /
sr where sr is the sampling rate.  F32 precision is an issue.

> g_05 =
>     let f = mouseX KR 220 880 Exponential 0.1
>         tr = impulse AR f 0
>         sr = sampleRate
>         x = phasor AR tr (two_pi * f / sr) 0 two_pi 0
>     in sin x * 0.1

phasor as lfSaw, but with precision issues

> g_06 = phasor AR (impulse AR 440 0) (2 * 440 / sampleRate) (-1) 1 0 * 0.2

> g_07 =
>   let ph = phasor AR (impulse AR 440 0) (two_pi * 440 / sampleRate) 0 two_pi 0
>   in sin ph * 0.2
