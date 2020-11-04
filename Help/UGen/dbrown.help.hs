-- dbrown
let n = dbrown 'α' dinf 0 15 1
    x = mouseX KR 1 40 Exponential 0.1
    t = impulse KR x 0
    f = demand t 0 n * 30 + 340
in sinOsc AR f 0 * 0.1

-- dbrown
let n = demand (impulse KR 10 0) 0 (dbrown 'α' dinf (-1) 1 0.05)
    f = linExp n (-1) 1 64 9600
in sinOsc AR f 0 * 0.1
