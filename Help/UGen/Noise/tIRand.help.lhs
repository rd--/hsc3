> import Sound.SC3.Monadic

> do { l <- tIRand (-1) 1 =<< dust KR 10
>    ; n <- pinkNoise AR
>    ; audition (out 0 (pan2 (n * 0.1) l 1)) }

> do { n <- tIRand 4 12 =<< dust KR 10
>    ; let f = n * 150 + (mce [0,1])
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }
