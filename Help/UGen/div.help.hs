-- div ; /
sinOsc ar 440 0 / 6

-- div ; creates a beating effect (subaudio rate)
(pinkNoise 'α' ar / fSinOsc kr 5 0) * 0.05

-- div ; optimises identity
sinOsc ar 440 0 / 1 * 0.1
