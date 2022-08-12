-- / ; division (floating point)
sinOsc ar 440 0 / 6

-- / ; creates a beating effect (subaudio rate)
(pinkNoiseId 'α' ar / fSinOsc kr 5 0) * 0.05

-- / ; optimises identity
sinOsc ar 440 0 / 1 * 0.1

-- div ; integer division
let d f = (sinOsc kr 0.25 0 * 200 + 500) `f` 17
in sinOsc ar (midiCps (mce2 (d div) (d (/)) * 3)) 0 * mce2 0.1 0.05
