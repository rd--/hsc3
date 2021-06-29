-- summer
sinOsc ar (X.summer (impulse kr 5 0) 1.5 0 0 * 100) 0 * 0.1

-- summer
let s_trig = dust 'α' kr 5
    s_reset = impulse kr 0.5 0
in sinOsc ar (X.summer s_trig (mce2 0.5 0.25) s_reset 1 * 100) 0 * 0.1
