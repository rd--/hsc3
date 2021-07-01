-- zOsc
let modulator = sinOsc kr 0.1 0
    freq = lfNoise2Id 'α' kr 10 `in_exprange` (10,1000)
    formantfreq = lfNoise2Id 'β' kr 1 `in_exprange` (700,1500)
    shape = modulator
    mode = sinOsc kr 0.91 0
    sig = X.zOsc ar freq formantfreq shape mode
in pan2 sig modulator 0.1
