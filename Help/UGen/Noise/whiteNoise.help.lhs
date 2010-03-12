whiteNoise rate

Generates noise whose spectrum has equal power at all frequencies.

> import Sound.SC3.Monadic

> audition . (out 0) . (* 0.05) =<< whiteNoise AR

Random filtered noise bursts.

> do { n <- whiteNoise AR
>    ; t <- dust AR (mce [3, 7])
>    ; f <- tExpRand 20 1800 t
>    ; bw <- tExpRand 0.001 1 t
>    ; let { e = decay2 t 0.01 0.2
>          ; r = resonz (n * e) f bw }
>      in audition (out 0 r) }

The same graph, without using do notation.

> whiteNoise AR >>= \n -> 
> dust AR (mce [3, 7]) >>= \t -> 
> tExpRand 20 1800 t >>= \f ->
> tExpRand 0.001 1 t >>= \bw -> 
> let { e = decay2 t 0.01 0.2
>     ; r = resonz (n * e) f bw }
> in audition (out 0 r)

 
