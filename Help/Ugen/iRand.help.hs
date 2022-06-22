-- iRand
let f = iRandId 'α' 200 1200
    e = line kr 0.2 0 0.1 RemoveSynth
in fSinOsc ar f 0 * e
