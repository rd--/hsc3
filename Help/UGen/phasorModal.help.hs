-- phasorModal
let input = lpf (dust2Id 'α' ar 3) 1500
    freq = control kr "freq" 100.0
    decay = control kr "decay" 0.25
    damp = control kr "damp" 1.0
    amp = control kr "amp" 0.5
    phase = control kr "phase" 0.0
in X.phasorModal input freq decay damp amp phase

-- phasorModal
let pitchstretch = -0.1
    f0 = 80
    k = 32
    n = mceFill
        k
        (\i -> let sig = lpf (dust2Id i ar (int_to_ugen (i + 1) / 3)) 1500
                   freq = (int_to_ugen i + 1) * (pitchstretch + 1) * f0
                   decay_ = randId i 0.225 0.35
                   damp = 1 -- control kr ("damp" ++ show i) 1
                   amp = lfNoise2Id i kr 1 `in_range` (0.25,0.45)
                   phase = 0
               in X.phasorModal sig freq decay_ damp amp phase)
in splay n (lfNoise2Id 'α' kr 0.1 `in_range` (0,1)) 1 0 True
