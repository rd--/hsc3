-- line ; SC3 reorders the mul and add inputs to precede the doneAction input
let f = line KR 200 17000 5 RemoveSynth
in sinOsc AR f 0 * 0.05

-- line ; demonstrate RemoveGroup done-action
let f = line KR 200 (mce2 209 211) 5 RemoveGroup
in sinOsc AR f 0 * 0.05
