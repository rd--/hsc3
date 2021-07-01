-- riRandN ; two channel sin tones ; harmonic series
sinOsc ar (55 * X.rRandNId 2 'α' 1 17) 0 * 0.1

-- riRandN ; 5-channel sin tones ; harmonic series
splay (sinOsc ar (55 * X.rRandNId 15 'α' 1 5) 0) 1 0.1 0 True

-- riRandN ; 15-channel sin tones ; harmonic series
splay (sinOsc ar (55 * X.rRandNId 12 'α' 1 17) 0) 1 0.1 0 True
