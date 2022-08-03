-- varSaw
let x = mouseX kr 0 1 Linear 0.2 in varSaw ar 220 0 x * 0.1

-- varSaw
let f = lfPulse kr (mce2 3 3.03) 0 0.3 * 200 + 200
    w = linLin (lfTri kr 1 0) (-1) 1 0 1
in varSaw ar f 0 w * 0.1

-- varSaw ; compare with lfPulse at ar
let f = lfPulse kr 3 0 0.3 * 200 + 200
in mce [varSaw ar f 0 0.2
       ,lfPulse ar f 0 0.2] * 0.1

-- varSaw ; per-note width modulation
let d = linLin (lfNoise2Id 'α' kr 0.1) (-1) 1 0.05 0.5
    t = impulse ar (1 / d) 0
    w0 = tRandId 'β' 0 0.35 t
    w1 = tRandId 'γ' 0.65 1 t
    w = phasor ar t ((w1 - w0) * sampleDur) w0 w1 0
    e = decay2 t 0.1 d
    f = midiCps (tRandId 'δ' 36 72 t)
    o = varSaw ar f 0 w * e * 0.1
    l = tRandId 'ε' (-1) 1 t
in pan2 o l 1

-- varSaw ; http://sc-users.bham.ac.narkive.com/sj4Tw3ub/sync-osc#post6
let freq = control kr "freq" 110
    factor = control kr "factor" 1
    x = mouseX kr 0 1.0 Linear 0.2
    y = mouseY kr (mce2 23 17) 0 Linear 0.2
    t = impulse kr 0.5 0
    ph = varSaw ar (freq * factor * tChooseId 'α' t (mce [0.125,0.5,1.3,1.5,23.0])) 0 x * y
in sinOsc ar (freq * mce2 1.001 1) ph * 0.1

-- varSaw ; slow indeterminate modulation of width, http://sccode.org/1-5as
let midinote = 60
    gate_ = 1
    amp = 0.25
    asr = envAsr 0.1 1 0.1 (EnvNum (-4))
    env = envGen kr gate_ 1 0 1 RemoveSynth asr
    freq = midiCps midinote
    width = range 0.2 0.8 (lfNoise2Id 'α' kr 1) * range 0.7 0.8 (sinOsc kr 5 (randId 'β' 0.0 1.0))
in varSaw ar freq 0 width * env * amp
