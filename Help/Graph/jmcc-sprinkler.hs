-- sprinkler (jmcc) #1
let f = lfPulse KR 0.09 0 0.16 * 10 + 7
    t = lfPulse KR f 0 0.25 * 0.1
    n = whiteNoise 'α' AR
in bpz2 (n * t)
