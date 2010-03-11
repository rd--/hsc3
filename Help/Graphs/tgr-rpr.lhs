tgr-rpr (rd)

> import Sound.OpenSoundControl
> import Sound.SC3.Monadic
> import System.Random

> main =
>   let { sf = "/home/rohan/audio/text.snd"
>       ; preset = [ 0.01, 0.02
>                  , 0.95, 1.05
>                  , 0.02, 0.06
>                  , 0.2, 0.3
>                  , 0.7, 0.9
>                  , -1.0, 1.0 ]
>       ; dustR r lo hi = do { n1 <- dwhite 1 lo hi
>                            ; n2 <- whiteNoise r
>                            ; d <- dseq dinf n1
>                            ; return (tDuty r d 0 DoNothing (abs n2) 1) }
>       ; rpr n t = tRand (in' 1 kr n) (in' 1 kr (n + 1)) t
>       ; rrand l r = getStdRandom (randomR (l, r))
>       ; rSet = [ (0.005, 0.025), (0.05, 0.25)
>                , (0.75,  0.95) , (1.05, 1.25)
>                , (0.001, 0.01) , (0.02, 0.04)
>                , (0.1,   0.2)  , (0.2, 0.4)
>                , (0.0,   0.45) , (0.55, 1.0)
>                , (-1.0,  0.0)  , (0.0, 1.0) ]
>       ; edit fd = do { s <- mapM (\(l,r) -> rrand l r) rSet
>                      ; send fd (c_setn [(0, s)])
>                      ; pauseThread 0.35 } }
>   in do { clk <- dustR ar (in' 1 kr 0) (in' 1 kr 1)
>         ; rat <- rpr 2 clk
>         ; dur <- rpr 4 clk
>         ; pos <- fmap (* (bufDur kr 10)) (rpr 8 clk)
>         ; pan <- rpr 10 clk
>         ; amp <- rpr 6 clk
>         ; withSC3 (\fd -> do { async fd (b_allocRead 10 sf 0 0)
>                              ; send fd (c_setn [(0, preset)])
>                              ; let o = tGrains 2 clk 10 rat pos dur pan amp 2
>                                in play fd (out 0 o)
>                              ; pauseThread 0.3
>                              ; sequence (replicate 16 (edit fd))
>                              ; reset fd }) }
