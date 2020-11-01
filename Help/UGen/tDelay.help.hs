-- tDelay
let z = impulse AR 2 0
    z' = tDelay z 0.5
    o = sinOsc AR 440 0 * 0.1
in mce [z * 0.1,toggleFF z' * o]
