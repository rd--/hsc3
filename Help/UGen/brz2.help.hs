-- brz2
let n = whiteNoise 'α' AR * 0.05
    x = mouseX KR (-1) 1 Linear 0.1
in xFade2 n (brz2 n) x 1
