pan2 in pos level

Two channel equal power panner.  The pan position is bipolar, -1 is
left, +1 is right.  The level is a control rate input.

> n <- pinkNoise AR
> audition (out 0 (pan2 n (fSinOsc KR 2 0) 0.3))

> n <- pinkNoise AR
> audition (out 0 (pan2 n (mouseX KR (-1) 1 Linear 0.2) (mouseY KR 0 1 Linear 0.2)))
