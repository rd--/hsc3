-- dswitch1 ; c.f. dswitch
let x = mouseX kr 0 4 Linear 0.1
    y = mouseY kr 1 15 Linear 0.1
    t = impulse kr 3 0
    w = dwhiteId 'α' dinf 20 23
    n = dswitch1Id 'β' x (mce [1, 3, y, 2, w])
    f = demand t 0 n * 30 + 340
in sinOsc ar f 0 * 0.1
