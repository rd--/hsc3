-- quadC
quadC AR (sampleRate / 2) 1 (-1) (-0.75) 0 * 0.1

-- quadC
let x = mouseX KR 3.5441 4 Linear 0.1
in quadC AR 4000 (negate x) x 0 0.1 * 0.2

-- quadC
let x = mouseX KR 3.5441 4 Linear 0.1
    f = quadC AR 4 (negate x) x 0 0.1 * 800 + 900
in sinOsc AR f 0 * 0.1

