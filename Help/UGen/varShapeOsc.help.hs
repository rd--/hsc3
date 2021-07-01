-- varShapeOsc
let modulator = sinOsc kr (lfNoise2Id 'a' kr 4 * 10) 0
    freq = 100
    pw = sinOsc kr 0.01 0 `in_range` (0,1)
    waveshape = lag modulator 0.1 `in_range` (0,1)
    sync_ = 1
    syncfreq = 182
    sig = X.varShapeOsc ar freq pw waveshape sync_ syncfreq
in pan2 sig modulator 0.1
