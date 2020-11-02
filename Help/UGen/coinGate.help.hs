-- coinGate
let g = coinGate 'α' 0.2 (impulse KR 10 0)
    f = tRand 'β' 300.0 400.0 g
in sinOsc AR f 0 * 0.1
