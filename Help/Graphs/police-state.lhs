police state (jmcc)

> import Sound.SC3.Monadic

> main =
>   let node = do { r0 <- rand 0.02 0.12
>                 ; r1 <- rand 0 (pi*2)
>                 ; r2 <- rand 0 600
>                 ; r3 <- rand 700 1300
>                 ; r4 <- rand (-1) 1
>                 ; r5 <- rand 80 120
>                 ; n0 <- lfNoise2 ar r5
>                 ; let f = sinOsc kr r0 r1 * r2 + r3
>                   in return (pan2 (sinOsc ar f 0 * n0 * 0.1) r4 1) }
>   in do { nodes <- clone 4 node
>         ; n0 <- clone 2 (lfNoise2 kr 0.4)
>         ; n1 <- lfNoise2 ar (n0 * 90 + 620)
>         ; n2 <- lfNoise2 kr (mce2 0.3 0.301)
>         ; let e = n1 * (n2 * 0.15 + 0.18)
>           in audition (out 0 (combL (mix nodes + e) 0.3 0.3 3)) }

{ var node = { var r0 = Rand.new(0.02, 0.12)
             ; var r1 = Rand.new(0, 2pi)
             ; var r2 = Rand.new(0, 600)
             ; var r3 = 1000 + Rand.new(-300, 300)
             ; var r4 = Rand.new(-1, 1)
             ; var r5 = 100 + Rand.new(-20, 20)
             ; var n0 = LFNoise2.ar(r5)
             ; var f = SinOsc.kr(r0, r1, r2, r3)
             ; Pan2.ar(SinOsc.ar(f, 0) * n0 * 0.1, r4, 1) }
; var n0 = LFNoise2.kr([0.4, 0.4])
; var n1 = LFNoise2.ar(n0 * 90 + 620)
; var n2 = LFNoise2.kr([0.3,0.3])
; var e = n1 * (n2 * 0.15 + 0.18)
; Out.ar(0, CombL.ar(Mix.fill(4, node) + e, 0.3, 0.3, 3)) }.play
