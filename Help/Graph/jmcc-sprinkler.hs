-- sprinkler (jmcc) #1
let f = lfPulse kr 0.09 0 0.16 * 10 + 7
    t = lfPulse kr f 0 0.25 * 0.1
    n = whiteNoise 'α' ar
in bpz2 (n * t)
