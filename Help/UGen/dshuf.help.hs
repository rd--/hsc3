-- dshuf
let a = dseqId 'α' dinf (dshufId 'β' 3 (mce [1,3,2,7,8.5]))
    x = mouseX kr 1 40 Exponential 0.1
    t = impulse kr x 0
    f = demand t 0 a * 30 + 340
in sinOsc ar f 0 * 0.1

-- dshuf
let a = dseqId 'α' dinf (dshufId 'β' 5 (X.randNId 81 'γ' 0 10))
    x = mouseX kr 1 10000 Exponential 0.1
    t = impulse ar x 0
    f = demand t 0 a * 30 + 340
in sinOsc ar f 0 * 0.1
