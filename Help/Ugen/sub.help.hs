-- sub (-)
let o = fSinOsc ar 800 0
    n = pinkNoiseId 'α' ar
in (o - n) * 0.1

-- sub ; dc offset
fSinOsc ar 440 0 * 0.1 - 0.5

-- sub ; optimise identity
(sinOsc ar 440 0 - 0) * 0.1
