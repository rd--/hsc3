h-chatter (rd)

> import Control.Monad
> import Sound.SC3.Monadic

> main =
>   let { wrp i l r = linLin i (-1) 1 l r
>       ; mma m a = return . (+ a)  . (* m)
>       ; h0 = do { n <- mma 5 5 =<< lfNoise0 kr 1
>                 ; a <- mma 0.2 1.2 =<< lfNoise2 kr n
>                 ; b <- mma 0.15 0.15 =<< lfNoise2 kr n
>                 ; let { f = 40
>                       ; h = henonN ar (mce2 f (f * 0.5)) a b 0 0 }
>                   in return (saw ar (h * 3200 + 1600) * 0.35) }
>       ; h1 = do { n0 <- lfNoise0 kr 32
>                 ; n1 <- lfNoise0 kr 2
>                 ; let { a = mouseX kr 1.2 1.4 Linear 0.1
>                       ; b = mouseY kr 0.2 0.3 Linear 0.1
>                       ; h = wrp n0 1 32
>                       ; p = wrp n1 2400 3200
>                       ; l = wrp n1 (-0.75) 0.75
>                       ; g = wrp n1 0.55 0.85
>                       ; f = 40
>                       ; o = blip ar (wrp (henonN ar f a b 0 0) p (p * 2)) h }
>                   in return (pan2 o l g * 0.35) } }
>   in audition . out 0 =<< liftM2 (+) h0 h1

{ var h0 = { var n = LFNoise0.kr(1, 5, 5)
           ; var a = LFNoise0.kr(1, 0.2, 1.2)
           ; var b = LFNoise0.kr(1, 0.15, 0.15)
           ; var f = 40
           ; var h = HenonN.ar([f, f * 0.5], a, b, 0, 0)
           ; Saw.ar(h * 3200 + 1600) * 0.35 }
; var h1 = { var n0 = LFNoise0.kr(32)
           ; var n1 = LFNoise0.kr(2)
           ; var a = MouseX.kr(1.2, 1.4, 'linear', 0.1)
           ; var b = MouseY.kr(0.2, 0.3, 'linear', 0.1)
           ; var h = n0.range(1, 32)
           ; var p = n1.range(2400, 3200)
           ; var l = n1.range(-0.75, 0.75)
           ; var g = n1.range(0.55, 0.85)
           ; var f = 40
           ; var o = Blip.ar(HenonN.ar(f, a, b, 0, 0).range(p, p * 2), h)
           ; Pan2.ar(o, l, g) * 0.35 }
; Out.ar(0, h0.value + h1.value) }.play
