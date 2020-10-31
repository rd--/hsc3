-- sub (-)
let o = fSinOsc AR 800 0
    n = pinkNoise 'α' AR
in (o - n) * 0.1

-- sub ; dc offset
fSinOsc AR 440 0 * 0.1 - 0.5

-- sub ; optimise identity
(sinOsc AR 440 0 - 0) * 0.1
