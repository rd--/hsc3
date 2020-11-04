-- moogVCF
let o = mix (lfSaw AR (mce2 120 180) 0 * 0.33)
    cf = linExp (lfCub KR 0.1 (0.5 * pi)) (-1) 1 180 8500
in X.moogVCF o cf 0.75

-- moogVCF
let o = pulse AR (mce2 40 121) (mce2 0.3 0.7)
    cf = range 30 4200 (sinOsc KR (range 0.001 2.2 (lfNoise0 'α' KR 0.42)) 0)
in X.moogVCF o cf 0.8 * 0.25
