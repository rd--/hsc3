-- delayL ; c.f. delayN & delayC
let t = mouseX kr 0.001 0.2 Exponential 0.2
    s = impulse ar 1 0
    d = delayL s 0.6 t
in mce2 d s
