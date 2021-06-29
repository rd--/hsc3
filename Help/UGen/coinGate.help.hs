-- coinGate
let g = coinGate 'α' 0.2 (impulse kr 10 0)
    f = tRand 'β' 300.0 400.0 g
in sinOsc ar f 0 * 0.1
