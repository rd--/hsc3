-- stkBandedWG ; default param
X.stkBandedWG AR 440.0 0.0 0.0 0.0 0.0 64.0 0.0 0.0 1.0

-- stkBandedWG ; default param ; instr=3
X.stkBandedWG AR 440.0 3.0 0.0 0.0 0.0 64.0 0.0 0.0 1.0

-- stkBandedWG
let tr = impulse KR (mouseX KR 0.5 12 Linear 0.2) 0 - 0.5
    freq = midiCPS (tExpRand 'α' 24 96 tr)
    instr = 3 -- Uniform Bar = 0, Tuned Bar = 1, Glass Harmonica = 2, Tibetan Bowl = 3
    modalresonance = tRand 'ζ' 0 127 tr
in X.stkBandedWG AR freq instr 0 0 0 modalresonance 0 0 tr

-- stkBandedWG ; texture=overlap,4,3,4,inf
let freq = rand 'α' 110 440
    instr = iRand 'β' 1 3
    bowpressure = iRand 'γ' 32 96
    bowmotion = rand 'δ' 32 96
    integration = rand 'ε' 0 64
    modalresonance = rand 'ζ' 32 96
    bowvelocity = rand 'η' 64 96
    setstriking = 127 -- setstriking: 0 = Plucked, 127 = Bowed
in X.stkBandedWG AR freq instr bowpressure bowmotion integration modalresonance bowvelocity setstriking 1
