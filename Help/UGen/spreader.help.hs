-- spreader
let theta = pi / 2
    filtsPerOctave = 8
in X.spreader AR (pinkNoise 'α' AR * 0.1) theta filtsPerOctave

-- spreader
let theta = mouseX KR 0 (pi / 2) Linear 0.2
    filtsPerOctave = 8
in X.spreader AR (pinkNoise 'α' AR * 0.1) theta filtsPerOctave

-- spreader
let in_ = sinOsc AR (mouseX KR 110 880 Exponential 0.2) 0 * 0.1
    theta = pi / 2
    filtsPerOctave = 8
in X.spreader AR in_ theta filtsPerOctave
