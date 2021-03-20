-- oteySoundBoard
let loc = mouseX KR (-1) 1 Linear 0.2
in X.oteySoundBoard (pan2 (soundIn 0) loc 0.1) 20 20 0.8

-- oteySoundBoard
let d = dust 'α' AR 1
    n = whiteNoise 'β' AR
    i = decay (d * 0.5) 0.2 * n
    loc = mouseX KR (-1) 1 Linear 0.2
    c1 = 20
    c3 = 20
    mix = 0.8
in X.oteySoundBoard (pan2 i loc 0.1) c1 c3 mix
