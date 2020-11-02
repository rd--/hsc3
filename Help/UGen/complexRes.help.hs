-- complexRes
let s = pulse AR 0.1 0.001 * 0.1
    fr = 50 + 5000 * sinOsc AR 50 0
    dt = 0.5
in X.complexRes s fr dt
