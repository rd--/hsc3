a / b

Division, written '/'.

Division can be tricky with signals because of division by zero.

> import Sound.SC3.ID

> let { o = fSinOsc KR 10 0.5
>     ; n = pinkNoise 'a' AR }
> in audition (out 0 ((n * 0.1) / (o * 0.75)))
