lastValue in diff

Output the last value before the input changed more than a threshhold.

> let x = mouseX KR 100 400 Linear 0.1
> audition $ sinOsc AR (lastValue x 40) 0 * 0.1

> let x = mouseX KR 0.1 4 Linear 0.1
> audition $ sinOsc AR (abs (lastValue x 0.5 - x) * 400 + 200) 0 * 0.2
