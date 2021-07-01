-- brz2
let n = whiteNoiseId 'α' ar * 0.05
    x = mouseX kr (-1) 1 Linear 0.1
in xFade2 n (brz2 n) x 1
