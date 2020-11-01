-- timer
let t = impulse KR (mouseX KR 0.5 20 Exponential 0.1) 0
in sinOsc AR (timer t * 500 + 500) 0 * 0.2
