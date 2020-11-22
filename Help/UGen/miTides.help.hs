-- MiTides ; shape
let shape = lfTri AR 0.11 0 `in_range` (0,1)
    c0 = head . mceChannels
in c0 (X.miTides AR 100 shape 0.5 0.5 0.2 0 0 {-output_mode:-} 2 1 9 1) * 0.25

-- MiTides ; slope
let slope = lfTri AR 0.11 0 `in_range` (0,1)
    c0 = head . mceChannels
in c0 (X.miTides AR 100 0.5 slope 0.5 0.2 0 0 {-output_mode:-} 2 1 9 1) * 0.25

-- MiTides ; phasing
let shape = 0.2
    slope_ = 0.0
    shift_ = lfNoise1 'α' AR 0.3 `in_range` (0,1) -- slightly shift phases
    smooth = lfNoise1 'β' KR 0.02 `in_range` (0.1,0.9)
    sig = X.miTides AR 70 shape slope_ smooth shift_ 0 0 {-output_mode:-} 2 1 9 1 * 0.15
in splay sig 1 1 0 True

-- MiTides ; chords ; tremolo
let shape = lfTri AR 0.11 0 `in_range` (0,1)
    slope_ = lfTri AR 0.2 0 `in_range` (0,1);
    chord = lfNoise1 'α' AR 0.1 `in_range` (0.5,1);
    smooth = sinOsc AR (lfNoise1 'β' KR 0.2 `in_range` (0.1,10)) 0 `in_range` (0,0.5)
    sig = X.miTides AR 200 shape slope_ smooth chord 0 0 {-output_mode:-} 3 1 9 1 * 0.25
in splay sig 1 1 0 True

-- MiTides ; amplitude mode
let freq = 0.15
    shape = 0.2
    slope_ = 0.2
    level = lfTri AR 0.1 0 `in_range` (0,1)
    env = X.miTides AR freq shape slope_ 0.3 level 0 0 {-output_mode:-} 1 {-ramp_mode:-} 1 9 1
    sines = sinOsc AR (mce [200,300,500,900]) 0
in splay (sines * env) 1 1 0 True

-- MiTides ; amplitude ; trigger input
let freq = lfNoise1 'α' AR 0.8 `in_range` (0.2,10)
    shape = 0.2
    slope_ = 0.2
    level = lfNoise1 'β' AR 0.3 `in_range` (0,1)
    tr = lfPulse AR 2 0 0.5
    env = X.miTides AR freq shape slope_ 0.3 level tr 0 {-output_mode:-} 1 {-ramp_mode:-} 0 9 1
    sines = sinOsc AR (mce [200,300,500,700]) 0
in splay (sines * env * 0.5) 1 1 0 True

-- MiTides ; phasing envs
let freq = 10
    shape = 0.2
    slope_ = 0.5
    shift_ = lfTri AR 0.1 0 `in_range` (0,1)
    trig = lfPulse AR 5 0 0.5
    env = X.miTides AR freq shape slope_ 0.5 shift_ trig 0 {-output_mode:-} 2 {-ramp_mode:-} 0 9 1
    sines = sinOsc AR (mce [200,300,500,700]) 0
in splay (sines * env * 0.25) 1 1 0 True

-- MiTides ; gate
let freq = 5 -- attack rate
    shape = lfTri AR 0.13 0 `in_range` (0,1)
    slope_ = 0.5
    smooth = lfTri AR 0.1 0 `in_range` (0,1)
    tr = lfPulse AR 1 0 0.5
    env = X.miTides AR freq shape slope_ smooth 0.8 tr 0 {-output_mode:-} 0 {-ramp_mode:-} 0 1 9
    sine = pmOsc AR 150 300 2 0
    c0 = head . mceChannels
in sine * c0 env

-- MiTides ; Lightbath -- Loom 4
let bpm = 140
    period = (60 / bpm) * 6
    speed = 1.0 / period
    del_times = period / mce2 6 8
    tr = impulse KR speed 0
    clock = phasor KR tr 1 0 1 0
    pit = tChoose 'α' tr (mce [67, 60, 65])
    shape = lfTri KR 0.11 0 `in_range` (0,1)
    slope_ = lfTri KR 0.11 0 `in_range` (0.5,0.8)
    smooth = lfNoise1 'β' KR 0.07 `in_range` (0.25,0.7)
    chord = lfNoise1 'γ' KR 0.03 `in_range` (0.0,0.5)
    oscs = X.miTides AR (midiCPS pit) shape slope_ smooth chord 0 0 3 1 9 1
    envs = X.miTides AR speed 0.35 0 0.46 0.92 0 clock 3 1 {-ratio:-} 9 {-rate:-} 0
    scaler = lfNoise1 'δ' KR (mce [0.1, 0.07, 0.11, 0.05])
    sig = splay (oscs * envs * scaler) 1 1 (lfTri KR 0.1 0) True
    del = combL sig 2 del_times 6 * 0.5
in sig + del
