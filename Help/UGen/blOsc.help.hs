-- blOsc
let freq = rand 'α' 55 220
    width = rand 'β' 0 1
    waveform = iRand 'γ' 0 2
in pan2 (X.blOsc ar freq width waveform) (rand 'δ' (-1) 1) (rand 'ε' 0.02 0.1)

-- blOsc ; k-rate (not implemented for width/waveform)
let tr = dust 'α' kr 2
    freq = tRand 'β' 55 220 tr
    width = tRand 'γ' 0 1 tr
    waveform = tiRand 'δ' 0 2 tr
in pan2 (X.blOsc ar freq width waveform) (tRand 'ε' (-1) 1 tr) (tRand 'ζ' 0.02 0.1 tr)

-- blOsc ; k-rate (not implemented for width/waveform)
let freq = control_m kr "freq" 110 (55,880,"exp")
    width = control_m kr "width" 0.5 (0,1,"lin")
    waveform = control_m kr "waveform" 0 (0,2,"lin")
in X.blOsc ar freq width waveform * 0.1
