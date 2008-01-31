h-chatter (rd)

> let { wrp i l r = linLin i (-1) 1 l r
>     ; h0 = do { n <- return . (+ 5.0)  . (* 5.0)  =<< lfNoise0 KR 1
>               ; a <- return . (+ 1.2)  . (* 0.2)  =<< lfNoise2 KR n
>               ; b <- return . (+ 0.15) . (* 0.15) =<< lfNoise2 KR n
>               ; let { f = 40
>                     ; h = henonN AR (mce2 f (f * 0.5)) a b 0 0 }
>                 in return (saw AR (h * 3200 + 1600) * 0.35) }
>     ; h1 = do { n0 <- lfNoise0 KR 32
>               ; n1 <- lfNoise0 KR 2
>               ; let { a = mouseX KR 1.2 1.4 Linear 0.1
>                     ; b = mouseY KR 0.2 0.3 Linear 0.1
>                     ; h = wrp n0 1 32
>                     ; p = wrp n1 2400 3200
>                     ; l = wrp n1 (-0.75) 0.75
>                     ; g = wrp n1 0.55 0.85
>                     ; f = 40
>                     ; o = blip AR (wrp (henonN AR f a b 0 0) p (p * 2)) h }
>                 in return (pan2 o l g * 0.35) } }
> in audition . out 0 =<< liftM2 (+) h0 h1
