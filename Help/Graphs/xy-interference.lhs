xy-interference (rd)

> let { x = mouseX KR 20 22000 Linear (mce2 0.005 0.025)
>     ; y = mouseY KR 20 22000 Linear (mce2 0.005 0.075)
>     ; nd = do { n <- lfNoise0 KR (MCE [5, 9])
>               ; let { a = sinOsc AR (x + n) 0
>                     ; b = sinOsc AR y 0 }
>                 in return (a * b) } }
> in audition . (out 0) . sum =<< replicateM 3 nd
