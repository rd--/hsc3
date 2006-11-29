mostChange a b

Output the input that changed most.

> n <- lfNoise0 KR 1
> let x = mouseX KR 200 300 Linear 0.1
> audition $ sinOsc AR (mostChange (n * 400 + 900) x) 0 * 0.1
