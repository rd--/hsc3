-- bufDelayN
let b = localBuf 'α' 1 44100
    o = sinOsc ar (lfNoise2 'β' kr 0.5 * 100 + 110) 0 * 0.05
    d = abs (lfNoise2 'γ' kr 0.25)
in mce2 o (bufDelayN b o d)
