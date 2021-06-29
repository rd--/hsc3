-- http://sccode.org/1-e (jl)
let d0 = lpf (dust2 ar (lfNoise1 kr 0.2 `in_range` (40,50))) 7000
    n0 = pinkNoise ar * (0.08 + lfNoise1 kr 0.3 * 0.02) + d0
    e0 = line kr 0 1 10 DoNothing
    p0 = tanh (3 * gVerb (hpf n0 400) 250 100 0.25 0.5 15 0.3 0.7 0.5 300 * e0)
    lfNoise1C r f = clip (lfNoise1 r f) 0 1
    n1 = pinkNoise ar * ((lfNoise1C kr 3 * lfNoise1C kr 2) ** 1.8)
    f1 = linExp (lfNoise1 kr 1) (-1) 1 100 2500
    e1 = line kr 0 0.7 30 DoNothing
    p1 = gVerb (tanh (lpf (10 * hpf n1 20) f1)) 270 30 0.7 0.5 15 0.5 0.7 0.5 300 * e1
in limiter (p0 + p1) 1 0.01 * 0.2

-- http://sccode.org/1-e (jl) ; id
let d0 = lpf (dust2Id 'γ' ar (lfNoise1Id 'δ' kr 0.2 `in_range` (40,50))) 7000
    n0 = pinkNoiseId 'α' ar * (0.08 + lfNoise1Id 'β' kr 0.3 * 0.02) + d0
    e0 = line kr 0 1 10 DoNothing
    p0 = tanh (3 * gVerb (hpf n0 400) 250 100 0.25 0.5 15 0.3 0.7 0.5 300 * e0)
    lfNoise1C e r f = clip (lfNoise1Id e r f) 0 1
    n1 = pinkNoiseId 'ε' ar * ((lfNoise1C 'ζ' kr 3 * lfNoise1C 'η' kr 2) ** 1.8)
    f1 = linExp (lfNoise1Id 'θ' kr 1) (-1) 1 100 2500
    e1 = line kr 0 0.7 30 DoNothing
    p1 = gVerb (tanh (lpf (10 * hpf n1 20) f1)) 270 30 0.7 0.5 15 0.5 0.7 0.5 300 * e1
in limiter (p0 + p1) 1 0.01 * 0.2
