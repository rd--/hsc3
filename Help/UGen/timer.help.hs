-- timer
let t = impulse kr (mouseX kr 0.5 20 Exponential 0.1) 0
in sinOsc ar (timer t * 500 + 500) 0 * 0.2
