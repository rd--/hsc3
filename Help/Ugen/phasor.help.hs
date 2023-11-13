-- phasor ; controls sine frequency, end frequency matches second sine
let rate = mouseX kr 0.2 2 Exponential 0.1
    tr = impulse ar rate 0
    sr = sampleRate
    x = phasor ar tr (rate / sr) 0 1 0
    f = mce [linLin x 0 1 600 1000, 1000]
in sinOsc ar f 0 * 0.1

-- phasor ; control two sine frequencies, mouse y controls resetPos of the second
let rate = mouseX kr 1 200 Linear 0.1
    tr = impulse ar rate 0
    sr = sampleRate
    x = phasor ar tr (rate / sr) 0 1 (mce2 0 (mouseY kr 0 1 Linear 0.2))
in sinOsc ar (x * 500 + 500) 0 * 0.1

-- phasor ; as phase input to bufRd ; requires=buf
let (b, nc) = (control kr "buf" 100, 2)
    ph = phasor ar 0 (bufRateScale kr b) 0 (bufFrames kr b) 0
in bufRd nc ar b ph Loop NoInterpolation

-- phasor ; audio rate oscillator as phase input to bufRd ; requires=buf (non-wavetable format)
let (b, nc) = (control kr "buf" 100, 2)
    f = 440
    fr = bufFrames kr b
    rt = f * (fr / sampleRate)
    ph = phasor ar b (rt * bufRateScale kr b) 0 fr 0
in bufRdL nc ar b ph Loop * 0.1

-- phasor ; as impulse with reset
let impulse_reset freq reset =
        let ph = phasor ar reset (freq / sampleRate) 0 1 0
        in hpz1 ph <** 0
    x = mouseX kr 0 1 Linear 0.2 >** 0.5
    ck = impulse ar 3 0
    im = impulse_reset 3 x
    x' = sinOsc ar 440 0 * x * 0.05
    im' = sinOsc ar 220 0 * decay2 (ck + im) 0.01 0.5 * 0.1
in mce2 x' im'

-- phasor ; as sinOsc
let f0 = midiCps (mouseX kr 36 96 Linear 0.2)
    ph = phasor ar 0 (f0 * sampleDur) 0 1 0 * two_pi
in sin ph * 0.1

-- phasor ; as sinOsc with trigger ; triggers cause discontinuities (precision?)
let f0 = midiCps (mouseX kr 36 96 Linear 0.2)
    ph = phasor ar (impulse ar f0 0) (f0 * sampleDur) 0 1 0 * two_pi
in sin ph * 0.1

-- phasor ; as saw
let f0 = midiCps (mouseX kr 36 96 Linear 0.2)
in phasor ar 0 (f0 * sampleDur) (-1) 1 0 * 0.1

-- phasor ; approximation of square wave ; sync ; http://listarc.bham.ac.uk/lists/sc-users/msg69869.html
let freq = mouseX kr 200 4000 Exponential 0.2
    syncFreq = mouseY kr 50 800 Exponential 0.2
    syncSig = sinOsc ar syncFreq 0
    phase = phasor ar syncSig (freq * sampleDur) 0 1 0 * 2 * pi
    k = 12000 * (sampleRate / 48000) / (freq * log10 freq)
    width = sinOsc kr 0.1 0 `in_range` (0,1)
    sinSig = sinOsc ar 0 phase - (width * 2 - 1)
    squareSig = tanh (sinSig * k)
in pan2 (leakDC squareSig 0.995) 0 0.05

---- ; load sound file to buffer zero
withSc3 (async (b_allocRead 0 (sfResolve "pf-c5.aif") 0 0))

---- ; allocate and generate (non-wavetable) buffer (see osc for wavetable oscillator)
withSc3 (mapM_ maybe_async [b_alloc 0 8192 1,b_gen_sine1 0 [Normalise,Clear] [1]])

---- ; drawings
UI.ui_sc3_scope 2 0 4096 1 "audio" 0
