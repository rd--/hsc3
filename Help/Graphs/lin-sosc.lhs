lin-sosc (rd)

> import Control.Concurrent
> import Control.Monad
> import Sound.OpenSoundControl
> import Sound.SC3
> import System.Random

> main =
>   let { n = 1024
>       ; x = mouseX KR 0.001 1.0 Linear 0.1
>       ; tblM b = playBuf 1 b (x * bufRateScale KR b) 0 0 Loop DoNothing
>       ; tblC b c = playBuf 1 b (in' 1 KR c * bufRateScale KR b) 0 0 Loop DoNothing
>       ; o = sinOsc AR (tblM 0) 0 * tblM 1 
>       ; co = clip2 (pan2 o (tblC 1 0) 0.025) 0.25
>       ; rrand (a, b) = getStdRandom (randomR (a,b))
>       ; choose l = fmap (l !!) (rrand (0, length l - 1))
>       ; iota 0 _ _ = []
>       ; iota n l s = l : iota (n - 1) (l + s) s
>       ; geom 0 _ _ = []
>       ; geom n i f = i : geom (n - 1) (i * f) f
>       ; lineTo n l r = iota n l ((r - l) / n)
>       ; xlineTo n l r = geom n l ((r / l) ** (1.0 / n))
>       ; twoPi = pi * 2.0
>       ; srng l r e = let m = (l - r ) / 2
>                      in m + l + (e * m) 
>       ; freq = [ lineTo n 440.0 444.0
>                , lineTo n 40.0 16000.0
>                , xlineTo n 40.0 16000.0
>                , map (srng 20 21000 . sin) (lineTo n 0.0 twoPi)
>                , map (srng 20 12000 . cos) (lineTo n 0.0 twoPi)
>                , map (srng 20 22000 . tan) (lineTo n 0.0 twoPi)
>                , map (srng 20 90 . tan) (lineTo n 0.0 twoPi) ]
>       ; ampl = [ lineTo n 0.1 1.0
>                , lineTo n 1.0 0.1
>                , lineTo n 0.5 0.01
>                , lineTo n 0.01 0.5
>                , xlineTo n 1.0 0.1
>                , xlineTo n 0.1 1.0
>                , map sin (lineTo n 0.0 twoPi)
>                , map cos (lineTo n 0.0 twoPi)
>                , map (* 0.001) (map tan (lineTo n 0 twoPi)) ]
>       ; sloc = [ 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075
>                , 0.1, 0.25, 0.5, 0.75
>                , 0.8, 0.85, 1.0, 1.005 ]
>       ; paus = [0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75]
>       ; update fd = do { f <- choose freq
>                        ; a <- choose ampl
>                        ; s <- choose sloc
>                        ; p <- choose paus
>                        ; send fd (b_setn 0 [(0, f)])
>                        ; send fd (b_setn 1 [(0, a)])
>                        ; send fd (c_set [(0, s)])
>                        ; pauseThread p } }
>   in do { withSC3 (\fd -> do { async fd (b_alloc 0 (floor n) 1)
>                              ; async fd (b_alloc 1 (floor n) 1)
>                              ; play fd (out 0 co) })
>         ; forkIO (withSC3 (\fd -> replicateM_ 128 (update fd))) }
