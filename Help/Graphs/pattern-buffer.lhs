pattern buffer (rd)

> import Sound.SC3.Monadic
> import System.Random

> main =
>   let { nf = 2 * 48000
>       ; c = 24
>       ; tseq l = let n = fromIntegral (length l) / 2.0
>                  in select (lfSaw kr 0.5 0 * n + n) (mce l)
>       ; rrand l r = getStdRandom (randomR (l, r))
>       ; p = phasor ar 0 (bufRateScale kr 10) 0 (bufFrames kr 10) 0
>       ; t = bufRdC 1 ar 10 p Loop 
>       ; rs h = do { r0 <- rrand 0 nf
>                   ; r1 <- rrand 0.0 1.0
>                   ; send h (b_set1 10 r0 r1) } }
>   in do { r1 <- sequence (replicate c (rrand 36 96))
>         ; r2 <- sequence (replicate c (rrand (-1.0) 1.0))
>         ; r3 <- rrand 0 1
>         ; n1 <- tRand 0.02 0.08 t
>         ; let { e = decay2 t 0.01 n1
>               ; f = midiCPS (tseq r1)
>               ; l = tseq r2
>               ; o = [sinOsc ar f 0, saw ar f] !! r3 }
>           in withSC3 (\fd -> do { async fd (b_alloc 10 (nf * 2) 1)
>                                 ; sequence (replicate c (rs fd))
>                                 ; play fd (out 0 (pan2 o l e)) }) }
