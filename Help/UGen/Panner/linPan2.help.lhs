linPan2 in pos level

Two channel linear pan.  See Pan2.

> n <- pinkNoise AR
> audition (out 0 (linPan2 n (fSinOsc KR 2 0) 0.1))

> audition (out 0 (linPan2 (fSinOsc AR 800 0) (fSinOsc KR 3 0) 0.1))
