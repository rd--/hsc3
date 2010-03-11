tgb (rd)

> import Sound.SC3.Monadic
> import System.Random

> main =
>   let { mkls bp t = envGen kr 1 1 0 1 RemoveSynth (envCoord bp t 1 EnvLin)
>       ; pm_t l r d t = let { le = mkls l d
>                            ; re = mkls r d }
>                        in tRand le re t
>       ; wrp i l r = linLin i (-1) 1 l r
>       ; pm_n rt l d = let { le = mkls l d
>                           ; re = mkls l d }
>                       in do { n <- whiteNoise rt
>                             ; return (wrp n le re) }
>       ; gb b d = do { gps <- pm_n ar [(0, 400), (1, 900)] d
>                     ; let { t = impulse ar gps 0
>                           ; pm_f (l, r) = pm_t l r d t }
>                       in do { du <- pm_f ([(0, 0.005), (0.5, 0.015), (1, 0.005)]
>                                          ,[(0, 0.009), (0.5, 0.020), (1, 0.009)])
>                             ; pn <- pm_f ([(0, -1.0), (0.5, -0.5), (1, 0.5)]
>                                          ,[(0, -0.5), (0.5, 0.5), (1, 1.0)])
>                             ; rt <- pm_f ([(0, 6), (0.5, 12), (1, 6)]
>                                          ,[(0, 12), (0.5, 12), (1, 12)])
>                             ; cs <- pm_f ([(0, 0), (1, 0.95)]
>                                          ,[(0, 0), (1, 1)])
>                             ; am <- pm_f ([(0, 0.25), (0.5, 0.55), (1, 0.15)]
>                                          ,[(0, 0.5), (0.5, 0.75), (1, 0.25)])
>                             ; let cs' = cs * (bufDur kr b)
>                               in return (tGrains 2 t b rt cs' du pn am 2) } }
>       ; fn = "/home/rohan/audio/text.snd" }
>   in withSC3 (\fd -> do { async fd (b_allocRead 10 fn 0 0)
>                         ; play fd . (out 0) =<< gb 10 12 })
