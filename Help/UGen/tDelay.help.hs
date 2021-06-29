-- tDelay
let z = impulse ar 2 0
    z' = tDelay z 0.5
    o = sinOsc ar 440 0 * 0.1
in mce [z * 0.1,toggleFF z' * o]
