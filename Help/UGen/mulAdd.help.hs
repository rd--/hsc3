-- mulAdd
mulAdd (sinOsc AR 440 0) 0.1 0.05

-- mulAdd ; optimisations
sinOsc AR 440 0 * 0.1 + 0.05

-- mulAdd ; optimisations
0.05 + sinOsc AR 440 0 * 0.1

-- mulAdd ; optimisations
0.05 + 0.1 * sinOsc AR 440 0

-- mulAdd ; all AR inputs optimise ordinarily
(sinOsc AR 440 0 * sinOsc AR 441 0 + sinOsc AR 442 0) * 0.1

-- mulAdd
(sinOsc AR 440 0 + sinOsc AR 441 0 * sinOsc AR 442 0) * 0.1
