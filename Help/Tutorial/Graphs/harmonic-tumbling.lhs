harmonic tumbling

> let { t = xLine KR (mce2 10 10) 0.1 60 DoNothing
>     ; o h = do { n <- dust KR t
>                ; r <- rand 0.25 0.5
>                ; let e = decay2 (n * 0.02) 0.005 r
>                  in return (fSinOsc AR (80 * h) 0 * e) } }
> in audition . out 0 . sum =<< mapM o [1..11]
