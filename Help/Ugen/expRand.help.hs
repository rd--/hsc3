-- expRand
let a = line kr 0.5 0 0.01 RemoveSynth
    f = expRandId 'α' 100.0 8000.0
in fSinOsc ar f 0 * a
