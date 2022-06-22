-- lfBrownNoise0
let n = X.lfBrownNoise0Id 'α' ar 10 0.05 0
    f = linExp n (-1) 1 64 9600
in sinOsc ar f 0 * 0.1
