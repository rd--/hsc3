-- summer
sinOsc AR (X.summer KR (impulse KR 5 0) 1.5 0 0 * 100) 0 * 0.1

-- summer
let s_trig = dust 'α' KR 5
    s_reset = impulse KR 0.5 0
in sinOsc AR (X.summer KR s_trig (mce2 0.5 0.25) s_reset 1 * 100) 0 * 0.1
