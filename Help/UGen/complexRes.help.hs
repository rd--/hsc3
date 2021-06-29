-- complexRes
let s = pulse ar 0.1 0.001 * 0.1
    fr = 50 + 5000 * sinOsc ar 50 0
    dt = 0.5
in X.complexRes s fr dt
