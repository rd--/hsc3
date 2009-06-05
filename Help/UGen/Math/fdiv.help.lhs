a / b

Division, written '/'.

Division can be tricky with signals because of division by zero.

> import Sound.SC3

> let o = fSinOsc KR 10 0.5
> in do { n <- pinkNoise AR
>       ; audition (out 0 ((n * 0.1) / (o * 0.75))) }
