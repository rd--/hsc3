-- dibrown ; c.f. dbrown
let n = dibrown 'α' dinf 0 15 1
    x = mouseX KR 1 40 Exponential 0.1
    t = impulse KR x 0
    f = demand t 0 n * 30 + 340
in sinOsc AR f 0 * 0.1
