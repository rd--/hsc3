-- pulseDivider
let p = impulse ar 8 0
    d = pulseDivider p (mce [4,7]) 0
    a = sinOsc ar 1200 0 * decay2 p 0.005 0.1
    b = sinOsc ar  600 0 * decay2 d 0.005 0.5
in (a + b) * 0.1
