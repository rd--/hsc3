-- quadC
quadC ar (sampleRate / 2) 1 (-1) (-0.75) 0 * 0.1

-- quadC
let x = mouseX kr 3.5441 4 Linear 0.1
in quadC ar 4000 (negate x) x 0 0.1 * 0.2

-- quadC
let x = mouseX kr 3.5441 4 Linear 0.1
    f = quadC ar 4 (negate x) x 0 0.1 * 800 + 900
in sinOsc ar f 0 * 0.1

