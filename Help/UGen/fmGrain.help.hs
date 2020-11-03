-- fmGrain
let t = impulse AR 20 0
    n = linLin (lfNoise1 'α' KR 1) (-1) 1 1 10
    s = envSine 9 0.1
    e = envGen KR 1 1 0 1 RemoveSynth s
in X.fmGrain t 0.2 440 220 n * e
