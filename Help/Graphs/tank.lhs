tank (jmcc)

> import Control.Monad
> import Sound.SC3.Monadic

> main =
>   let { r_allpass i = do { r <- clone 2 (rand 0.005 0.02)
>                          ; return (allpassN i 0.03 r 1) }
>       ; chain n f = foldl (>=>) return (replicate n f)
>       ; pling = do { d <- dust ar 0.2
>                    ; f <- expRand 300 2200
>                    ; p <- rand (-1) 1
>                    ; let { s1 = cubed (fSinOsc ar f 0)
>                          ; s2 = decay2 d 0.1 0.5 * 0.1 * s1 }
>                      in return (pan2 s2 p 1) }
>       ; bang = do { d <- dust ar 0.01
>                   ; n <- brownNoise ar
>                   ; return (pan2 (decay2 d 0.04 0.3 * n) 0 1) }
>       ; tank i = do { r1 <- clone 2 (rand 0.01 0.05)
>                     ; r2 <- clone 2 (rand 0.03 0.15)
>                     ; let { l0 = localIn 2 ar * 0.98
>                           ; l1 = onePole l0 0.33
>                           ; [l1l, l1r] = mceChannels l1
>                           ; l2 = rotate2 l1l l1r 0.23
>                           ; l3 = allpassN l2 0.05 r1 2
>                           ; l4 = delayN l3 0.3 (mce [0.17, 0.23])
>                           ; l5 = allpassN l4 0.05 r2 2
>                           ; l6 = leakDC l5 0.995
>                           ; l7 = l6 + i }
>                       in return (mrg [l7, localOut l7]) }
>       ; signal = do { s <- liftM2 (+) bang (mixFillM 8 (const pling))
>                     ; chain 4 r_allpass s } }
>   in audition . out 0 =<< tank =<< signal

{ var r_allpass = { arg signal
                  ; var r = { Rand.new(0.005, 0.02) }.dup
                  ; AllpassN.ar(signal, 0.03, r, 1); }
; var pling = { var d = Dust.ar(0.2)
              ; var f = ExpRand.new(300, 2200)
              ; var p = Rand.new(-1, 1)
              ; var s1 = FSinOsc.ar(f, 0).cubed
              ; var s2 = Decay2.ar(d, 0.1, 0.5) * 0.1 * s1
              ; Pan2.ar(s2, p, 1) }
; var bang = { var d = Dust.ar(0.01)
             ; var n = BrownNoise.ar
             ; Pan2.ar(Decay2.ar(d, 0.04, 0.3) * n, 0, 1) }
; var tank = { arg i
             ; var r1 = { Rand.new(0.01,0.05) }.dup
             ; var r2 = { Rand.new(0.03,0.15) }.dup
             ; var l0 = LocalIn.ar(2) * 0.98
             ; var l1 = OnePole.ar(l0, 0.33)
             ; var l2 = Rotate2.ar(l1[0], l1[1], 0.23)
             ; var l3 = AllpassN.ar(l2, 0.05, r1, 2)
             ; var l4 = DelayN.ar(l3, 0.3, [0.17,0.23])
             ; var l5 = AllpassN.ar(l4, 0.05, r2, 2)
             ; var l6 = LeakDC.ar(l5, 0.995)
             ; var l7 = l6 + i
             ; LocalOut.ar(l7)
             ; l7 }
; var signal = Mix.fill(12, pling) + bang.value
; 4.do({ signal = r_allpass.value(signal) })
; Out.ar(0, tank.value(signal)) }.play

http://create.ucsb.edu/pipermail/sc-users/2004-April/009692.html
