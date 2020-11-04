-- gate
let t = lfPulse AR 1 0 0.1 in gate (fSinOsc AR 500 0 * 0.25) t
