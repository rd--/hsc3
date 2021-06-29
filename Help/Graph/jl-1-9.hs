-- http://sccode.org/1-9 (jl)
let n = 956 -- 0
    k = 99 -- 1
    a = hpf (pinkNoise ar * 0.005) 10 * line kr 0 1 9 DoNothing
    f i = ringz (a * lfNoise1 kr (0.05 + rand 0 0.1)) (55 * (i + n) + 60) 0.2
in tanh (gVerb (mixFill k f) 70 99 0.5 0.5 15 1 0.7 0.5 300)

-- http://sccode.org/1-9 (jl) ; id
let n = 956 -- 0
    k = 98 -- 1
    a = hpf (pinkNoiseId 'α' ar * 0.005) 10 * line kr 0 1 9 DoNothing
    f (z,i) = ringz (a * lfNoise1Id z kr (0.05 + randId z 0 0.1)) (55 * (i + n) + 60) 0.2
in tanh (gVerb (sum (map f (zip ['β' ..] [0 .. k]))) 70 99 0.5 0.5 15 1 0.7 0.5 300)
