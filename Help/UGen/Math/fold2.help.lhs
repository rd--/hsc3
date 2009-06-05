fold2 a b

Bilateral folding.  Folds a to +/- b.

> import Sound.SC3

> let { o = fSinOsc AR 1000 0
>     ; l = line KR 0 1 8 DoNothing }
> in audition (out 0 (fold2 o l))
