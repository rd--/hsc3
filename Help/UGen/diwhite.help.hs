-- dwhite ; c.f. dwhite
let n = diwhite 'α' dinf (-7) 7
    x = mouseX KR 1 40 Exponential 0.1
    t = impulse KR x 0
    f = midiCPS (demand t 0 n + 60)
in sinOsc AR f 0 * 0.1
