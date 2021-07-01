-- blOsc
let freq = randId 'α' 55 220
    width = randId 'β' 0 1
    waveform = iRandId 'γ' 0 2
in pan2 (X.blOsc ar freq width waveform) (randId 'δ' (-1) 1) (randId 'ε' 0.02 0.1)

-- blOsc ; k-rate (not implemented for width/waveform)
let tr = dustId 'α' kr 2
    freq = tRandId 'β' 55 220 tr
    width = tRandId 'γ' 0 1 tr
    waveform = tiRandId 'δ' 0 2 tr
in pan2 (X.blOsc ar freq width waveform) (tRandId 'ε' (-1) 1 tr) (tRandId 'ζ' 0.02 0.1 tr)

-- blOsc ; k-rate (not implemented for width/waveform)
let freq = control_m kr "freq" 110 (55,880,"exp")
    width = control_m kr "width" 0.5 (0,1,"lin")
    waveform = control_m kr "waveform" 0 (0,2,"lin")
in X.blOsc ar freq width waveform * 0.1
