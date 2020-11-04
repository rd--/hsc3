-- peak
let t = dust 'α' AR 20
    r = impulse AR 0.4 0
    f = peak t r * 500 + 200
in sinOsc AR f 0 * 0.2
