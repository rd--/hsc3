-- harmonicOsc
let modulator = sinOsc KR 0.1 0
    freq = modulator `in_exprange` (10,1000)
    firstharmonic = 3
    amplitudes = X.rRandN 16 'α' 0.01 0.1
    sig = X.harmonicOsc AR freq firstharmonic amplitudes
in pan2 sig modulator 0.2
