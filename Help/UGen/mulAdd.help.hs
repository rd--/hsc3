-- mulAdd
mulAdd (sinOsc ar 440 0) 0.1 0.05

-- mulAdd ; optimisations
sinOsc ar 440 0 * 0.1 + 0.05

-- mulAdd ; optimisations
0.05 + sinOsc ar 440 0 * 0.1

-- mulAdd ; optimisations
0.05 + 0.1 * sinOsc ar 440 0

-- mulAdd ; all ar inputs optimise ordinarily
(sinOsc ar 440 0 * sinOsc ar 441 0 + sinOsc ar 442 0) * 0.1

-- mulAdd
(sinOsc ar 440 0 + sinOsc ar 441 0 * sinOsc ar 442 0) * 0.1
