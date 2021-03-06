-- specFlatness
let z = soundIn 0
    g = 1 {- gain, set as required -}
    a = pollExt 1 (X.wAmp kr z 0.05) 0 (label "a")
    f = fft' (localBufId 'α' 2048 1) z
    c = pollExt 1 (specCentroid kr f) 0 (label "c")
    w = pollExt 1 (specFlatness kr f) 0 (label "w")
in bpf (pinkNoiseId 'a' ar) c w * a * g
