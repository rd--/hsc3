-- freqShift ; shifting a 100Hz tone by 1 Hz rising to 500Hz
let i = sinOsc ar 100 0
    s = xLine kr 1 500 5 RemoveSynth
in freqShift i s 0 * 0.1

-- freqShift ; Shifting a complex tone by 1 Hz rising to 500Hz
let d = klangSpec [101, 303, 606, 808] [1, 1, 1, 1] [1, 1, 1, 1]
    i = klang ar 1 0 d
    s = xLine kr 1 500 5 RemoveSynth
in freqShift i s 0 * 0.1

-- freqShift ; Modulating shift and phase
let s = lfNoise2 'α' ar 0.3
    i = sinOsc ar 10 0
    p = linLin (sinOsc ar 500 0) (-1) 1 0 (2 * pi)
in freqShift i (s * 1500) p * 0.1

-- freqShift ; Shifting bandpassed noise
let n1 = whiteNoise 'α' ar
    n2 = lfNoise0 'β' ar 5.5
    i = bpf n1 1000 0.001
    s = n2 * 1000
in freqShift i s 0 * 32

-- freqShift
let e = lfGauss ar 4 (1/8) 0 Loop DoNothing
    o = blip ar 60 4 * e
    a = o / 4 + localIn 2 ar 0
    s = freqShift a (lfNoise0 'α' kr (1/4) * 90) 0
    z = delayC s 1 0.1 * 0.9
in mrg2 s (localOut z)
