-- ; rLagC
let x = mouseX kr 220 440 Linear 0.2
in sinOsc ar (mce2 x (X.rLagC x 1 0 5 0)) 0 * 0.1

-- ; rLagC
let p = range 220 440 (lfPulse kr 0.1 0 0.5)
    x = mouseX kr (-8) 8 Linear 0.2
    y = mouseY kr (-8) 8 Linear 0.2
in sinOsc ar (mce2 p (X.rLagC p 0.75 x 1.5 y)) 0 * 0.1

---- ; ar is not working
let o = lfPulse ar 50 0 0.25
in mce2 o (rLagC o (1/500) 0 (1/500) 0) * 0.2

let o = lfPulse ar 50 0 0.25
    x = mouseX kr 0.0 (1/100) Linear 0.2
    y = mouseY kr 0.0 (3/100) Linear 0.2
in mce2 o (rLagC o x 0 y 0) * 0.2

let o = sinOsc ar 50 0
in mce2 o (rLagC o 0.01 0 0.01 0) * 0.2
