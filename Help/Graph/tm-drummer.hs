-- drummer (tm)
let n = whiteNoise ar
    tempo = 4
    tr = impulse ar tempo 0
    tr_2 = pulseDivider tr 4 2
    tr_4 = pulseDivider tr 4 0
    snare = n * decay2 tr_2 0.005 0.5
    bass = sinOsc ar 60 0 * decay2 tr_4 0.005 0.5
    hihat = hpf n 10000 * decay2 tr 0.005 0.5
in pan2 (snare + bass + hihat) 0 0.4

-- drummer (tm) ; id
let n = whiteNoiseId 'α' ar
    tempo = 4
    tr = impulse ar tempo 0
    tr_2 = pulseDivider tr 4 2
    tr_4 = pulseDivider tr 4 0
    snare = n * decay2 tr_2 0.005 0.5
    bass = sinOsc ar 60 0 * decay2 tr_4 0.005 0.5
    hihat = hpf n 10000 * decay2 tr 0.005 0.5
in pan2 (snare + bass + hihat) 0 0.4
