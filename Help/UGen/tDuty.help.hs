-- tDuty ; rhythm
let d = dseqId 'α' dinf (mce [0.1, 0.2, 0.4, 0.3])
in tDuty ar d 0 DoNothing 1 0

-- tDuty ; amplitude changes
let d0 = dseqId 'α' dinf (mce [0.1, 0.2, 0.4, 0.3])
    d1 = dseqId 'β' dinf (mce [0.1, 0.4, 0.01, 0.5, 1.0])
in ringz (tDuty ar d0 0 DoNothing d1 1) 1000 0.1

-- tDuty ; mouse control
let d = dseqId 'α' dinf (mce [0.1, 0.4, 0.01, 0.5, 1.0])
    x = mouseX kr 0.003 1 Exponential 0.1
in ringz (tDuty ar x 0 DoNothing d 1) 1000 0.1 * 0.5

-- tDuty ; note that the 440 is the shorter pitch, since gap is set to false
let d0 = dserId 'α' 12 (mce [0.1, 0.3])
    d1 = dserId 'β' 12 (mce [440, 880])
    t = tDuty ar d0 0 RemoveSynth d1 0
in sinOsc ar (latch t t) 0 * 0.1

-- tDuty ; abstraction
let bp n d act = let (e,t) = unzip d
                     mkId z l = dserId z n (mce l)
                     sq = tDuty ar (mkId 'α' t) 0 act (mkId 'β' e) 0
                 in latch sq sq
    bp' d = bp (genericLength d) d
    tm m = let f (e,t) = (e,t * m) in map f
    f1 = midiCPS (bp 35 (tm 0.125 [(60,1),(63,1),(67,2),(68,1),(62,1)]) RemoveSynth)
    f2 = midiCPS (bp' [(60,1),(63,0.5),(67,0.5),(68,1),(62,1)] DoNothing)
in sinOsc ar (mce2 f1 f2) 0 * 0.1
