-- dwhite ; c.f. dwhite
let n = diwhite 'α' dinf (-7) 7
    x = mouseX kr 1 40 Exponential 0.1
    t = impulse kr x 0
    f = midiCPS (demand t 0 n + 60)
in sinOsc ar f 0 * 0.1
