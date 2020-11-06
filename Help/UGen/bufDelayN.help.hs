-- bufDelayN
let b = localBuf 'α' 1 44100
    o = sinOsc AR (lfNoise2 'β' KR 0.5 * 100 + 110) 0 * 0.05
    d = abs (lfNoise2 'γ' KR 0.25)
in mce2 o (bufDelayN b o d)
