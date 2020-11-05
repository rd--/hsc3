-- ; rLagC
let x = mouseX KR 220 440 Linear 0.2
in sinOsc AR (mce2 x (X.rLagC x 1 0 5 0)) 0 * 0.1

-- ; rLagC
let p = range 220 440 (lfPulse KR 0.1 0 0.5)
    x = mouseX KR (-8) 8 Linear 0.2
    y = mouseY KR (-8) 8 Linear 0.2
in sinOsc AR (mce2 p (X.rLagC p 0.75 x 1.5 y)) 0 * 0.1

---- ; AR is not working
let o = lfPulse AR 50 0 0.25
in mce2 o (rLagC o (1/500) 0 (1/500) 0) * 0.2

let o = lfPulse AR 50 0 0.25
    x = mouseX KR 0.0 (1/100) Linear 0.2
    y = mouseY KR 0.0 (3/100) Linear 0.2
in mce2 o (rLagC o x 0 y 0) * 0.2

let o = sinOsc AR 50 0
in mce2 o (rLagC o 0.01 0 0.01 0) * 0.2
