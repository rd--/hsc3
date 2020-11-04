-- linRand ; c.f. hsc3-lang/Sound/SC3/Lang/Random/Gen.hs:linrand
let f = linRand 'α' 200.0 10000.0 (mce [-1, 1])
    e = line KR 0.4 0 0.01 RemoveSynth
in fSinOsc AR f 0 * e


