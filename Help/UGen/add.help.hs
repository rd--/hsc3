-- add (+)
let o = fSinOsc ar 800 0
    n = pinkNoiseId 'α' ar
in (o + n) * 0.1

-- add ; dc offset
fSinOsc ar 440 0 * 0.1 + 0.5

-- add ; optimise identity (rhs)
(sinOsc ar 440 0 + 0) * 0.1

-- add ; optimise identity (lhs)
(0 + sinOsc ar 440 0) * 0.1
