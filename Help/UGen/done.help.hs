-- done ; high tone left or low tone right
let x = mouseX kr (-1) 1 Linear 0.1
    e = linen x 0.1 0.1 0.5 DoNothing
    o1 = sinOsc ar 880 0 * 0.1
    o2 = sinOsc ar 440 0 * e
in mce [done e * o1,o2]
