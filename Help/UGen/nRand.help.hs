-- nRand
let n = nRandId 'α' 1200.0 4000.0 (mce [2,5])
    e = line kr 0.2 0 0.1 RemoveSynth
in fSinOsc ar n 0 * e
