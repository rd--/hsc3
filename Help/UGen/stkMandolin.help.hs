-- stkMandolin
let x = mouseX kr 0.25 16 Linear 0.2
    tr = impulse kr x 0 - 0.5
    freq = midiCPS (tiRandId 'α' 32 96 tr)
    bodysize = tRandId 'β' 0 127 tr
    pickposition = tRandId 'γ' 0 127 tr
    stringdamping = tRandId 'δ' 0 127 tr
    stringdetune = tRandId 'ε' 0 127 tr
    aftertouch = tRandId 'ζ' 0 127 tr
    m = X.stkMandolin ar freq bodysize pickposition stringdamping stringdetune aftertouch tr
in pan2 m (tRandId 'η' (-1) 1 tr) 0.5

-- stkMandolin
let x = mouseX kr 3 16 Linear 0.2
    tr = impulse kr x 0 - 0.5 -- trig
    tr3 = pulseDivider tr 3 0
    freq = midiCPS (tiRandId 'α' 54 66 tr)
    bodysize = tRandId 'β' 72 94 tr3
    pickposition = tRandId 'γ' 32 42 tr3
    stringdamping = tRandId 'δ' 64 72 tr3
    stringdetune = tRandId 'ε' 0 4 tr3
    aftertouch = tRandId 'ζ' 2 8 tr3
    m = X.stkMandolin ar freq bodysize pickposition stringdamping stringdetune aftertouch tr
in pan2 m (tRandId 'η' (-1) 1 tr) 0.5

