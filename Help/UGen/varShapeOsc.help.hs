-- varShapeOsc
let modulator = sinOsc KR (lfNoise2 'a' KR 4 * 10) 0
    freq = 100
    pw = sinOsc KR 0.01 0 `in_range` (0,1)
    waveshape = lag modulator 0.1 `in_range` (0,1)
    sync_ = 1
    syncfreq = 182
    sig = X.varShapeOsc AR freq pw waveshape sync_ syncfreq
in pan2 sig modulator 0.1
