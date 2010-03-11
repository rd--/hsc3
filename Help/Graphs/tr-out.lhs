tr-out (rd)

> import Sound.SC3.Monadic

> main =
>   let node n = do { t <- dust kr 1.6
>                   ; r1 <- tRand 0 6 t
>                   ; r2 <- tRand 0 6 t
>                   ; r3 <- tRand 0 6 t
>                   ; let { f = midiCPS (bufRdN 1 kr 0 r1 NoLoop)
>                         ; p = bufRdN 1 kr 1 r2 NoLoop
>                         ; a = bufRdN 1 kr 2 r3 NoLoop }
>                     in return ( pan2 (sinOsc ar f 0) p a
>                               , sendTrig t n (f / 660) ) }
>   in withSC3 (\fd -> do { async fd (b_alloc 0 6 1)
>                         ; send fd (b_setn1 0 0 [60, 62, 64, 65, 67, 69])
>                         ; async fd (b_alloc 1 6 1)
>                         ; send fd (b_setn1 1 0 [-1, -0.5, 0, 0.25, 0.75, 1.0])
>                         ; async fd (b_alloc 2 6 1)
>                         ; send fd (b_setn1 2 0 [0.01, 0.05, 0.1, 0.15, 0.25, 0.35])
>                         ; ns <- mapM node [1..4]
>                         ; let o = sum (map fst ns)
>                           in play fd (out 0 (mrg (o : map snd ns))) })
