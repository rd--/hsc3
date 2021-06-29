-- tBrownRand
let t = dust 'α' kr 10
    dist = mouseX kr 0 5 Linear 0.2
    f = X.tBrownRand 'β' 300 3000 1 dist t
in sinOsc ar f 0 * 0.1

-- tBrownRand
let t = dust 'α' kr 10
    n = X.tBrownRand 'β' 0 1 0.2 0 t
    f = linExp n 0 1 300 3000
    o = sinOsc ar f 0
    l = X.tBrownRand 'γ' (-1) 1 1 4 t
in pan2 o l 0.1

-- tBrownRand ; audio rate noise
let x = mouseX kr 500 5000 Exponential 0.2
    y = mouseY kr 10 500 Exponential 0.2
    t = dust 'α' ar x
in lag (X.tBrownRand 'β' (-1) 1 0.2 0 t) (y / 48000)
