-- nRand
let n = nRand 'α' 1200.0 4000.0 (mce [2,5])
    e = line KR 0.2 0 0.1 RemoveSynth
in fSinOsc AR n 0 * e
