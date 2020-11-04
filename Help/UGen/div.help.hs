-- div ; /
sinOsc AR 440 0 / 6

-- div ; creates a beating effect (subaudio rate)
(pinkNoise 'α' AR / fSinOsc KR 5 0) * 0.05

-- div ; optimises identity
sinOsc AR 440 0 / 1 * 0.1
