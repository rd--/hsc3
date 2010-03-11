overlap-add (jmcc, rd)

> import Control.Concurrent
> import Sound.OpenSoundControl
> import Sound.SC3.Monadic

> main =
>   let { at t f 
>         = let at' t' f' = do { n <- f' t'
>                              ; pauseThreadUntil (t' + n)
>                              ; at' (t' + n) f' }
>           in do { pauseThreadUntil t
>                 ; at' t f
>                 ; return () }
>       ; mk_env a s 
>         = let { c = EnvNum 4
>               ; p = envLinen' a s a 1 (c, c, c) }
>           in envGen KR 1 1 0 1 RemoveSynth p
>       ; with_env g a s 
>         = out 0 (g * (mk_env a s))
>       ; overlap_add fd n o a s g 
>         = do { t <- utcr
>              ; let { g' = with_env g (constant a) (constant s)
>                    ; dt = (a + s + a) / o 
>                    ; f _ = do { send fd (s_new n (-1) AddToTail 1 [])
>                               ; return dt } }
>                in do { async fd (d_recv (synthdef n g'))
>                      ; at t f } }
>       ; oSine fd 
>         = do { r0 <- expRand 500 1400
>              ; r1 <- rand (-1) 1
>              ; let g = pan2 (sinOsc AR r0 0) r1 0.01
>                in overlap_add fd "random-sines" 4 2 4 g }
>       ; oNoise fd 
>         = do { n0 <- whiteNoise AR
>              ; r0 <- expRand 800 8400
>              ; r1 <- rand (-1) 1
>              ; let g = pan2 (resonz (n0 * 0.1) r0 0.05) r1 0.25
>                in overlap_add fd "random-noise" 4 2 4 g } }
>   in do { forkIO (withSC3 (\fd -> oSine fd))
>         ; forkIO (withSC3 (\fd -> oNoise fd)) }
