-- varSaw
let x = mouseX KR 0 1 Linear 0.2 in varSaw AR 220 0 x * 0.1

-- varSaw
let f = lfPulse KR (mce2 3 3.03) 0 0.3 * 200 + 200
    w = linLin (lfTri KR 1 0) (-1) 1 0 1
in varSaw AR f 0 w * 0.1

-- varSaw ; compare with lfPulse at AR
let f = lfPulse KR 3 0 0.3 * 200 + 200
in mce [varSaw AR f 0 0.2
       ,lfPulse AR f 0 0.2] * 0.1

-- varSaw ; per-note width modulation
let d = linLin (lfNoise2 'α' KR 0.1) (-1) 1 0.05 0.5
    t = impulse AR (1 / d) 0
    w0 = tRand 'β' 0 0.35 t
    w1 = tRand 'γ' 0.65 1 t
    w = phasor AR t ((w1 - w0) * sampleDur) w0 w1 0
    e = decay2 t 0.1 d
    f = midiCPS (tRand 'δ' 36 72 t)
    o = varSaw AR f 0 w * e * 0.1
    l = tRand 'ε' (-1) 1 t
in pan2 o l 1

-- varSaw ; http://sc-users.bham.ac.narkive.com/sj4Tw3ub/sync-osc#post6
let freq = control KR "freq" 110
    factor = control KR "factor" 1
    x = mouseX KR 0 1.0 Linear 0.2
    y = mouseY KR (mce2 23 17) 0 Linear 0.2
    t = impulse KR 0.5 0
    ph = varSaw AR (freq * factor * tChoose 'α' t (mce [0.125,0.5,1.3,1.5,23.0])) 0 x * y
in sinOsc AR (freq * mce2 1.001 1) ph * 0.1

-- varSaw ; slow indeterminate modulation of width, http://sccode.org/1-5as
let midinote = 60
    gate_ = 1
    amp = 0.25
    asr = envASR 0.1 1 0.1 (EnvNum (-4))
    env = envGen KR gate_ 1 0 1 RemoveSynth asr
    freq = midiCPS midinote
    width = range 0.2 0.8 (lfNoise2 'α' KR 1) * range 0.7 0.8 (sinOsc KR 5 (rand 'β' 0.0 1.0))
in varSaw AR freq 0 width * env * amp
