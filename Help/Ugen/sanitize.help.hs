-- sanitize ; when frequency is nan set to 220
let freq = tChoose (impulse kr 1 0) (mce2 440 (0 / 0))
in sinOsc ar (sanitize freq 220) 0 * 0.1
