-- iRand
let f = iRand 'α' 200 1200
    e = line KR 0.2 0 0.1 RemoveSynth
in fSinOsc AR f 0 * e
