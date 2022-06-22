-- dwrand
let n = dwrandId 'α' dinf (mce [0.3,0.2,0.1,0.2,0.2]) (mce [1,3,2,7,8])
    x = mouseX kr 1 400 Exponential 0.1
    t = impulse kr x 0
    f = demand t 0 n * 30 + 340
in sinOsc ar f 0 * 0.1
