-- mulAdd
mulAdd (sinOsc ar 440 0) 0.1 0.05

-- mulAdd ; optimisation of constant inputs of one and zero
mulAddOptimised (sinOsc ar 440 0) 1 0 * 0.1

-- mulAdd ; optimisation ; of * followed by +
sinOsc ar 440 0 * 0.1 + 0.05

-- mulAdd ; optimisation ; of + followed by * ; ugen at left
0.05 + sinOsc ar 440 0 * 0.1

-- mulAdd ; optimisations ; of + followed by * ; ugen at right
0.05 + 0.1 * sinOsc ar 440 0

-- mulAdd ; all ar inputs optimise ordinarily ; * then +
(sinOsc ar 440 0 * sinOsc ar 441 0 + sinOsc ar 442 0) * 0.1

-- mulAdd ; + then *
(sinOsc ar 440 0 + sinOsc ar 441 0 * sinOsc ar 442 0) * 0.1
