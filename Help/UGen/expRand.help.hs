-- expRand
let a = line KR 0.5 0 0.01 RemoveSynth
    f = expRand 'α' 100.0 8000.0
in fSinOsc AR f 0 * a
