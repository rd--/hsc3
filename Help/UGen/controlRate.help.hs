-- controlRate ; play a sine tone at control rate, the reciprocal of controlDur
let f = mce2 controlRate (recip controlDur)
in sinOsc AR f 0 * 0.1
