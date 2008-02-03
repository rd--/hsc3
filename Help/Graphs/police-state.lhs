police state (jmcc)

> let node = do { r0 <- rand 0.020 0.102
>               ; r1 <- rand 0 (pi*2)
>               ; r2 <- rand 0 600
>               ; r3 <- rand 700 1300
>               ; r4 <- rand (-1) 1
>               ; r5 <- rand 80 120
>               ; n0 <- lfNoise2 AR r5
>               ; let f = sinOsc KR r0 r1 * r2 + r3
>                 in return (pan2 (sinOsc AR f 0 * n0 * 0.1) r4 1) }
> in do { nodes <- clone 4 node
>       ; n0 <- clone 2 (lfNoise2 KR 0.4)
>       ; n1 <- lfNoise2 AR (n0 * 90 + 620)
>       ; n2 <- lfNoise2 KR (mce2 0.3 0.301)
>       ; let e = n1 * (n2 * 0.15 + 0.18)
>         in audition (out 0 (combL (mix nodes + e) 0.3 0.3 3)) }

{ var node = { var r0 = 0.1.rand + 0.02
             ; var r1 = 2pi.rand
             ; var r2 = 600.rand
             ; var r3 = 1000 + 300.rand2
             ; var r4 = 1.0.rand2
             ; var r5 = 100 + 20.0.rand2
             ; var n0 = LFNoise2.ar(r5)
             ; var f = SinOsc.kr(r0, r1, r2, r3)
             ; Pan2.ar(SinOsc.ar(f, 0, n0 * 0.1), r4, 1) }
; var n0 = LFNoise2.kr([0.4, 0.4])
; var n1 = LFNoise2.ar(n0 * 90 + 620)
; var n2 = LFNoise2.kr([0.3,0.3])
; var e = n1 * (n2 * 0.15 + 0.18)
; Out.ar(0, CombL.ar(Mix.fill(4, node) + e, 0.3, 0.3, 3)) }.play
