nharm (rd)

> import Control.Concurrent
> import Control.Monad
> import Sound.SC3.Monadic
> import System.Random

> main =
>   let { nharm n f = map ((* f) . fromIntegral) [1..n]
>       ; rrand l r = getStdRandom (randomR (l, r))
>       ; threadPause n = when (n>0) (threadDelay (floor (n * 1e6)))
>       ; klg m u = do { n <- rrand 4 u
>                      ; d <- rrand 9 12
>                      ; f <- rrand m (m + 2)
>                      ; l <- sequence (replicate n (rrand 0.01 0.02))
>                      ; p <- rrand (-1.0) 1.0
>                      ; let { a = 0.5
>                            ; e = envGen kr 1 0.9 0 1 RemoveSynth (envSine d a)
>                            ; nh = nharm n (midiCPS f)
>                            ; s = klangSpec nh l (replicate n 0.0) }
>                        in return (pan2 (klang ar 1 0 s) p e) }
>       ; ply :: Int -> (Double, Double) -> UGen -> Int -> IO ()
>       ; ply n (l,r) m u = replicateM_ n (do { threadPause =<< rrand l r
>                                             ; audition . out 0 =<< klg m u }) }
>   in do { forkIO (ply 32 (0.25, 0.75) 92 24)
>         ; forkIO (ply 8 (1.25, 1.75) 12 54) }
