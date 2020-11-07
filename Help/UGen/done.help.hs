-- done ; high tone left or low tone right
let x = mouseX KR (-1) 1 Linear 0.1
    e = linen x 0.1 0.1 0.5 DoNothing
    o1 = sinOsc AR 880 0 * 0.1
    o2 = sinOsc AR 440 0 * e
in mce [done e * o1,o2]
