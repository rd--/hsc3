-- pitch
let x = mouseX kr 220 660 Linear 0.1
    y = mouseY kr 0.05 0.25 Linear 0.1
    s = sinOsc ar x 0 * y
    a = amplitude kr s 0.05 0.05
    f = pitch s 440 60 4000 100 16 7 0.02 0.5 1 0
in mce [s, sinOsc ar (mceChannel 0 f / 2) 0 * a]

-- pitch ; live input tracking, carelessly
let s = hpf (soundIn 0) 90
    a = lag (amplitude kr s 0.01 0.01) 0.2
    [f,_] = mceChannels (pitch s 440 60 4000 100 16 1 0.02 0.5 1 0)
    fq = midiCps (roundE (lag (cpsMidi f) 0.1))
in mce [s * 0.1, lfTri ar f 0 * lag a 0.2 * lag (f >** 90 * f <** 500) 0.2]

-- pitch ; comparison of input frequency (x) and tracked oscillator frequency (f)
let x = mouseX kr 440 880 Exponential 0.1
    o = sinOsc ar x 0 * 0.1
    [f,_] = mceChannels (pitch o 440 60 4000 100 16 7 0.02 0.5 1 0)
    r = sinOsc ar f 0 * 0.1
    t = impulse kr 4 0
    pf = poll t f 0 (label "f")
    px = poll t x 0 (label "x")
in mce [out 0 (mce2 o r),pf,px]

-- pitch ; control param
let initFreq = control kr "initFreq" 440.0
    minFreq = control kr "minFreq" 60.0
    maxFreq = control kr "maxFreq" 4000.0
    execFreq = control kr "execFreq" 100.0
    maxBinsPerOctave = control kr "maxBinsPerOctave" 16.0
    median = control kr "median" 1.0
    ampThreshold = control kr "ampThreshold" 0.01
    peakThreshold = control kr "peakThreshold" 0.5
    downSample = control kr "downSample" 1.0
    clar = control kr "clar" 0.0
    p = pitch (soundIn 0) initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar
in sinOsc ar (mceChannel 0 p) 0 * 0.1
