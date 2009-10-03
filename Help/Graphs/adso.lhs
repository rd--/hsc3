adso (rd)

> import Sound.OpenSoundControl
> import Sound.SC3
> import System.Random

> main =
>   let { rrand l r = getStdRandom (randomR (l, r))
>       ; rrand_l j l r = sequence (replicate j (rrand l r))
>       ; n = 24
>       ; adso = let { get b j = let k = mce [0 .. constant j - 1]
>                                in bufRdN 1 kr b k NoLoop
>                    ; m = sinOsc kr (get 3 n) 0
>                    ; f = midiCPS (get 0 n) * (m * get 4 n + 1)
>                    ; l = get 2 n
>                    ; g = get 1 n }
>                in out 0 (mix (pan2 (sinOsc ar f 0) l g))
>       ; pattern fd t = do { z <- do { l <- rrand 22 48
>                                     ; r <- rrand 54 122
>                                     ; sequence (replicate n (rrand l r)) }
>                           ; send fd (b_setn1 0 0 z)
>                           ; let rn i l r = do { d <- rrand_l n l r
>                                               ; send fd (b_setn1 i 0 d) }
>                             in do { rn 1 0 0.1
>                                   ; rn 2 (-1) 1
>                                   ; rn 3 2 12
>                                   ; rn 4 0.001 0.0075
>                                   ; rn 5 1 24
>                                   ; rn 6 0.05 2.4 }
>                           ; pauseThread t } }
>   in withSC3 (\fd ->   do { mapM_ (\i ->   async fd (b_alloc i n 1)) [0..6]
>                         ; play fd adso
>                         ; mapM_ (pattern fd) =<< rrand_l 32 0.025 0.75
>                         ; reset fd })
