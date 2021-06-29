-- sprinkler mouse (jmcc) #1
let n = whiteNoise 'α' ar
    f = mouseX kr 0.2 50 Linear 0.2
    t = lfPulse kr f 0 0.25 * 0.1
in bpz2 (n * t)
