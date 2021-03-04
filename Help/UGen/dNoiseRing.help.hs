-- dNoiseRing
let tr = impulse AR 10 0
    x = mouseX KR 0 1 Linear 0.2
    y = mouseY KR 0 1 Linear 0.2
    nr = demand tr 0 (X.dNoiseRing 'α' x y 1.0 32.0 0.0)
    freq = midiCPS (linLin nr 0 (2 ** 32) 40 (40 + 48))
in sinOsc AR freq 0 * 0.1
