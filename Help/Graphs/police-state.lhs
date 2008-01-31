police state (jmcc)

> let node = do { r0 <- rand 0.02 0.1
>               ; r1 <- rand 0 (pi*2)
>               ; r2 <- rand 0 600
>               ; r3 <- rand 700 1300
>               ; r4 <- rand (-1) 1
>               ; r5 <- rand 80 120
>               ; n0 <- lfNoise2 AR r5
>               ; let f = sinOsc KR r0 r1 * r2 + r3
>                 in return (pan2 (sinOsc AR f 0 * n0 * 0.1) r4 1) }
> in do { nodes <- clone 4 node
>       ; n0 <- lfNoise2 KR (mce2 0.4 0.401)
>       ; n1 <- lfNoise2 AR (n0 * 90 + 620)
>       ; n2 <- lfNoise2 KR (mce2 0.3 0.301)
>       ; let e = n1 * (n2 * 0.15 + 0.18)
>         in audition (out 0 (combL (mix nodes + e) 0.3 0.3 3)) }
