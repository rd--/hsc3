-- phasor ; controls sine frequency, end frequency matches second sine
let rate = mouseX KR 0.2 2 Exponential 0.1
    tr = impulse AR rate 0
    sr = sampleRate
    x = phasor AR tr (rate / sr) 0 1 0
    f = mce [linLin x 0 1 600 1000, 1000]
in sinOsc AR f 0 * 0.1

-- phasor ; control two sine frequencies, mouse y controls resetPos of the second
let rate = mouseX KR 1 200 Linear 0.1
    tr = impulse AR rate 0
    sr = sampleRate
    x = phasor AR tr (rate / sr) 0 1 (mce2 0 (mouseY KR 0 1 Linear 0.2))
in sinOsc AR (x * 500 + 500) 0 * 0.1

-- phasor ; as phase input to bufRd
let b = control KR "buf" 0
    ph = phasor AR 0 (bufRateScale KR b) 0 (bufFrames KR b) 0
in bufRd 1 AR b ph Loop NoInterpolation

-- phasor ; audio rate oscillator as phase input to bufRd
let b = control KR "buf" 0
    f = 440
    fr = bufFrames KR b
    rt = f * (fr / sampleRate)
    ph = phasor AR b (rt * bufRateScale KR b) 0 fr 0
in bufRdL 1 AR b ph Loop * 0.1

-- phasor ; as impulse with reset
let impulse_reset freq reset =
        let ph = phasor AR reset (freq / sampleRate) 0 1 0
        in hpz1 ph <** 0
    x = mouseX KR 0 1 Linear 0.2 >** 0.5
    ck = impulse AR 3 0
    im = impulse_reset 3 x
    x' = sinOsc AR 440 0 * x * 0.05
    im' = sinOsc AR 220 0 * decay2 (ck + im) 0.01 0.5 * 0.1
in mce2 x' im'

-- phasor ; frequency: rate = (end - start) * freq / sample-rate ; precision is an issue.
let f = mouseX KR 220 880 Exponential 0.1
    tr = impulse AR f 0
    sr = sampleRate
    x = phasor AR tr (two_pi * f / sr) 0 two_pi 0
in sin x * 0.1

-- phasor ; as lfSaw, but with precision issues
phasor AR (impulse AR 440 0) (2 * 440 / sampleRate) (-1) 1 0 * 0.1

-- phasor ; as sinOsc, but with precision issues
let f0 = 220
    ph = phasor AR (impulse AR f0 0) (two_pi * f0 / sampleRate) 0 two_pi 0
in sin ph * 0.1

-- phasor ; approximation of square wave ; sync ; http://listarc.bham.ac.uk/lists/sc-users/msg69869.html
let freq = mouseX KR 200 4000 Exponential 0.2
    syncFreq = mouseY KR 50 800 Exponential 0.2
    syncSig = sinOsc AR syncFreq 0
    phase = phasor AR syncSig (freq * sampleDur) 0 1 0 * 2 * pi
    k = 12000 * (sampleRate / 48000) / (freq * log10 freq)
    width = sinOsc KR 0.1 0 `in_range` (0,1)
    sinSig = sinOsc AR 0 phase - (width * 2 - 1)
    squareSig = tanh (sinSig * k)
in pan2 (leakDC squareSig 0.995) 0 0.05

---- ; load sound file to buffer zero
withSC3 (async (b_allocRead 0 "/home/rohan/data/audio/pf-c5.aif" 0 0))

---- ; allocate and generate (non-wavetable) buffer (see osc for wavetable oscillator)
withSC3 (mapM_ maybe_async [b_alloc 0 8192 1,b_gen_sine1 0 [Normalise,Clear] [1]])
