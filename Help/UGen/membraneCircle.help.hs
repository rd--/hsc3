-- membraneCircle ; X = tension and impulse frequency, Y = duration, release-time and amplitude
let x = mouseX KR 0 1 Linear 0.2
    y = mouseY KR 1e-9 1 Exponential 0.2
    loss = linLin y 0 1 0.999999 0.999
    wobble = sinOsc KR 2 0
    tension = linLin x 0 1 0.01 0.1 + (wobble * 0.0001)
    p = envPerc 0.0001 y
    tr = impulse KR (linLin x 0 1 3 9) 0
    e = envGen KR tr (linLin y 0 1 0.05 0.25) 0 0.1 DoNothing p
in X.membraneCircle AR (pinkNoise 'α' AR * e) tension loss
