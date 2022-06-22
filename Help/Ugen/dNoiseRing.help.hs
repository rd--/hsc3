-- dNoiseRing
let tr = impulse ar 10 0
    x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
    nr = demand tr 0 (X.dNoiseRingId 'α' x y 1.0 32.0 0.0)
    freq = midiCps (linLin nr 0 (2 ** 32) 40 (40 + 48))
in sinOsc ar freq 0 * 0.1
