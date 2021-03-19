-- neoFormant
let modulator = sinOsc KR 0.1 0
    formantfreq = 150
    carrierfreq = lfNoise2 'α' KR 10 `in_exprange` (100,550)
    phaseshift = modulator `in_range` (0,1)
    sig = X.neoFormant AR formantfreq carrierfreq phaseshift
in pan2 sig modulator 0.1
