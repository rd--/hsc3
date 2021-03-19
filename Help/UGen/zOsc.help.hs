-- zOsc
let modulator = sinOsc KR 0.1 0
    freq = lfNoise2 'α' KR 10 `in_exprange` (10,1000)
    formantfreq = lfNoise2 'β' KR 1 `in_exprange` (700,1500)
    shape = modulator
    mode = sinOsc KR 0.91 0
    sig = X.zOsc AR freq formantfreq shape mode
in pan2 sig modulator 0.1
