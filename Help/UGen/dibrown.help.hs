-- dibrown ; c.f. dbrown
let n = dibrown 'α' dinf 0 15 1
    x = mouseX kr 1 40 Exponential 0.1
    t = impulse kr x 0
    f = demand t 0 n * 30 + 340
in sinOsc ar f 0 * 0.1
