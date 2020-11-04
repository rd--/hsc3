-- specFlatness
let z = soundIn 0
    g = 1 {- gain, set as required -}
    a = poll' 1 (X.wAmp KR z 0.05) 0 (label "a")
    f = fft' (localBuf 'α' 2048 1) z
    c = poll' 1 (specCentroid KR f) 0 (label "c")
    w = poll' 1 (specFlatness KR f) 0 (label "w")
in bpf (pinkNoise 'a' AR) c w * a * g
