pan2 in pos level

Two channel equal power panner.  The pan position is bipolar, -1 is
left, +1 is right.  The level is a control rate input.

> do { n <- pinkNoise AR
>    ; audition (out 0 (pan2 n (fSinOsc KR 2 0) 0.3)) }

> do { n <- pinkNoise AR
>    ; let { x = mouseX KR (-1) 1 Linear 0.2
>          ; y = mouseY KR 0 1 Linear 0.2 }
>      in audition (out 0 (pan2 n x y)) }
