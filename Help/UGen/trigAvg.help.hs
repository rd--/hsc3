-- trigAvg
let x = mouseX kr 0 1000 Linear 0.2
    b = mouseButton kr 0 1 0.2
    n = X.trigAvg kr (roundTo x 100) b
in sinOsc ar n 0 * 0.1

-- trigAvg
let n = X.trigAvg kr (sinOsc ar 0.1 0) (impulse kr 0.2 0)
in sinOsc ar 220 0 * n * 0.25
