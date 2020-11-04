-- inRange ; trigger noise burst
let n = brownNoise 'α' AR * 0.1
    x = mouseX KR 1 2 Linear 0.1
    o = sinOsc KR x 0 * 0.2
in inRange o (-0.15) 0.15 * n

-- inRange ; i-rate
sinOsc AR 440 0 * inRange (rand 'α' 0 1) 0.5 1.0 * 0.1
