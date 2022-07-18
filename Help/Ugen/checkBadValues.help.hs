-- checkBadValues ; when frequency is nan set gain to zero
let freq = tChoose (impulse kr 1 0) (mce2 440 (0 / 0))
    isNormal = checkBadValues freq 0 0 `equal_to` 0
in sinOsc ar freq 0 * 0.1 * isNormal
