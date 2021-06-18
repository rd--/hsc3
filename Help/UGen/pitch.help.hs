-- pitch
let x = mouseX KR 220 660 Linear 0.1
    y = mouseY KR 0.05 0.25 Linear 0.1
    s = sinOsc AR x 0 * y
    a = amplitude KR s 0.05 0.05
    f = pitch s 440 60 4000 100 16 7 0.02 0.5 1 0
in mce [s, sinOsc AR (mceChannel 0 f / 2) 0 * a]

-- pitch ; live input tracking, carelessly
let s = hpf (soundIn 0) 90
    a = lag (amplitude KR s 0.01 0.01) 0.2
    [f,_] = mceChannels (pitch s 440 60 4000 100 16 1 0.02 0.5 1 0)
    fq = midiCPS (roundE (lag (cpsMIDI f) 0.1))
in mce [s * 0.1, lfTri AR f 0 * lag a 0.2 * lag (f >** 90 * f <** 500) 0.2]

-- pitch ; comparison of input frequency (x) and tracked oscillator frequency (f)
let x = mouseX KR 440 880 Exponential 0.1
    o = sinOsc AR x 0 * 0.1
    [f,_] = mceChannels (pitch o 440 60 4000 100 16 7 0.02 0.5 1 0)
    r = sinOsc AR f 0 * 0.1
    t = impulse KR 4 0
    pf = poll t f 0 (label "f")
    px = poll t x 0 (label "x")
in mce [out 0 (mce2 o r),pf,px]

-- pitch ; control param
let initFreq = control KR "initFreq" 440.0
    minFreq = control KR "minFreq" 60.0
    maxFreq = control KR "maxFreq" 4000.0
    execFreq = control KR "execFreq" 100.0
    maxBinsPerOctave = control KR "maxBinsPerOctave" 16.0
    median = control KR "median" 1.0
    ampThreshold = control KR "ampThreshold" 0.01
    peakThreshold = control KR "peakThreshold" 0.5
    downSample = control KR "downSample" 1.0
    clar = control KR "clar" 0.0
    p = pitch (soundIn 0) initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar
in sinOsc AR (mceChannel 0 p) 0 * 0.1
