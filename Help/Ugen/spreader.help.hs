-- spreader
let theta = pi / 2
    filtsPerOctave = 8
in X.spreader ar (pinkNoiseId 'α' ar * 0.1) theta filtsPerOctave

-- spreader
let theta = mouseX kr 0 (pi / 2) Linear 0.2
    filtsPerOctave = 8
in X.spreader ar (pinkNoiseId 'α' ar * 0.1) theta filtsPerOctave

-- spreader
let in_ = sinOsc ar (mouseX kr 110 880 Exponential 0.2) 0 * 0.1
    theta = pi / 2
    filtsPerOctave = 8
in X.spreader ar in_ theta filtsPerOctave
