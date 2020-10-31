-- add (+)
let o = fSinOsc AR 800 0
    n = pinkNoise 'α' AR
in (o + n) * 0.1

-- add ; dc offset
fSinOsc AR 440 0 * 0.1 + 0.5

-- add ; optimise identity (rhs)
(sinOsc AR 440 0 + 0) * 0.1

-- add ; optimise identity (lhs)
(0 + sinOsc AR 440 0) * 0.1
