-- coinGate
let g = coinGateId 'α' 0.2 (impulse kr 10 0)
    f = tRandId 'β' 300.0 400.0 g
in sinOsc ar f 0 * 0.1
