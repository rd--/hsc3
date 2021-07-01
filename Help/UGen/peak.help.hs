-- peak
let t = dustId 'α' ar 20
    r = impulse ar 0.4 0
    f = peak t r * 500 + 200
in sinOsc ar f 0 * 0.2
