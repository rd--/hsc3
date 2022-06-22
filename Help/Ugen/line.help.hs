-- line ; NOTE - Sc3 reorders the mul and add inputs to precede the doneAction input
let f = line kr 200 17000 5 RemoveSynth
in sinOsc ar f 0 * 0.05

-- line ; demonstrate RemoveGroup done-action
let f = line kr 200 (mce2 209 211) 5 RemoveGroup
in sinOsc ar f 0 * 0.05
