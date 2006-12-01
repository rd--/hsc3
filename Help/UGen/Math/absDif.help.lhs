absDif a b

Calculates the value of (abs (- a b). Finding the magnitude of the
difference of two values is a common operation.

> audition $ fSinOsc ar 440 0 * absDif 0.2 (fSinOsc ar 2 0 * 0.5)
