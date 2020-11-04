-- trigAvg
let x = mouseX KR 0 1000 Linear 0.2
    b = mouseButton KR 0 1 0.2
    n = X.trigAvg KR (roundTo x 100) b
in sinOsc AR n 0 * 0.1

-- trigAvg
let n = X.trigAvg KR (sinOsc AR 0.1 0) (impulse KR 0.2 0)
in sinOsc AR 220 0 * n * 0.25
