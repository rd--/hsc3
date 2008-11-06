diffraction (rd)

> import Control.Monad
> import Sound.SC3

> let { p = let { x = mouseX kr 0.001 0.02 Exponential 0.1
>               ; y = mouseY kr 120 400 Exponential 0.1 }
>           in do { f <- fmap (* mce2 32 64) (lfNoise0 kr 4)
>                 ; w <- fmap (* x) (lfNoise0 kr 32)
>                 ; z <- fmap (* 0.1) (lfNoise0 kr 2)
>                 ; m <- lfNoise0 kr 6
>                 ; let s = pulse ar f w
>                   in return (resonz s (y + z) (m * 0.4 + 0.8) * 0.5) }
>     ; q = do { n <- lfNoise0 kr 128
>              ; s <- p
>              ; return (combN s 0.2 (n * 0.1 + 0.1) 3) }
>     ; r = let { x = mouseX kr 0.75 1.25 Exponential 0.1
>               ; y = mouseY kr 0.25 1 Exponential 0.1
>               ; f _ = do { fr <- fmap (* x) (rand 50 59)
>                          ; am <- fmap (* y) (rand 0.04 0.16)
>                          ; return (sinOsc ar fr 0 * am) } }
>           in liftM2 mce2 (mixFillM 16 f) (mixFillM 12 f) }
> in audition . (out 0) . sum =<< sequence [p, q, r]
