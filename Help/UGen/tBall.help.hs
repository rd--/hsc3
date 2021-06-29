-- tBall ; x = step noise modulation rate ; y = gravity
let sf = lfNoise0 'α' ar (mouseX kr 0.5 100 Exponential 0.2)
    g = mouseY kr 0.01 10 Exponential 0.2
    t = tBall ar sf g 0.01 0.002
in ringz (t * 4) (mce2 600 645) 0.3

-- tBall ; x = sine modulation rate ; y = friction
let fr = mouseX kr 1 1000 Exponential 0.2
    h = mouseY kr 0.0001 0.001 Exponential 0.2
    g = lfNoise0 'α' kr 0.1 * 3 + 5
    f = tBall ar (sinOsc ar fr 0) g 0.01 h
in pan2 (ringz f 1400 0.04) 0 5

-- tBall ; x = friction ; y = gravity
let fr = linExp (sinOsc kr 0.1 0) (-1) 1 1 600
    h = mouseY kr 0.0001 0.001 Exponential 0.2
    g = mouseX kr 1 10 Exponential 0.2
    f = tBall ar (sinOsc ar fr 0) g 0.01 h
in pan2 (ringz f 1400 0.04) 0 5
