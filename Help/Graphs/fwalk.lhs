fwalk (rd)

> import Sound.SC3.Monadic

> main =
>   let { n = [ 40.0, 47.0, 42.0, 40.0, 50.0
>             , 43.0, 35.0, 43.0, 40.0, 47.0
>             , 45.0, 35.0, 43.0, 42.0, 59.0
>             , 48.0, 40.0, 47.0, 52.0, 45.0 ]
>       ; m = [ 40.0, 40.0, 42.0, 47.0, 50.0
>             , 35.0, 43.0, 43.0, 40.0, 45.0
>             , 42.0, 35.0, 48.0, 47.0, 43.0
>             , 40.0, 59.0, 45.0, 47.0, 52.0 ] 
>       ; a = map (\b -> b_alloc b 20 1) [0, 1]
>       ; s = map (\(b, d) -> b_setn1 b 0 d) [(0, n), (1, m)]
>       ; fwalk r = do { t <- dust kr 3
>                      ; r1 <- tiRand 0 6 t
>                      ; r2 <- tRand (-0.0001) 0.0001 t
>                      ; let { f = bufRdL 1 kr (mce2 0 1) r1 NoLoop
>                            ; f' = f + r2
>                            ; o1 = blip ar (midiCPS (r + f)) 12
>                            ; o2 = blip ar (midiCPS (r + f')) 12 }
>                        in return ((o1 + o2) * decay2 t 0.3 1.2 * 0.1) } }
>   in withSC3 (\fd -> do { f1 <- fwalk 24
>                         ; f2 <- fwalk 36
>                         ; mapM_ (async fd) a
>                         ; mapM_ (send fd) s
>                         ; play fd (out 0 (f1 + f2)) })
