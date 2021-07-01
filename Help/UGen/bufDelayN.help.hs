-- bufDelayN
let b = localBufId 'α' 1 44100
    o = sinOsc ar (lfNoise2Id 'β' kr 0.5 * 100 + 110) 0 * 0.05
    d = abs (lfNoise2Id 'γ' kr 0.25)
in mce2 o (bufDelayN b o d)
