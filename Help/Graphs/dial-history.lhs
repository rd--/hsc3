dial history (jrhb)

> let { mfv = [[697, 770, 852, 941], [1209, 1336, 1477, 1633]]
>     ; numbers = [[3, 1]] ++ [[a, b] | a <- [0..2], b <- [0..2]]
>     ; range s l r = linLin s 0 1 l r
>     ; mce_r = mce . map mce
>     ; dinf = 1E9 }
> in do { n <- dwhite dinf 7 12
>       ; w <- dwhite 1 2 7
>       ; b <- dbrown n 0.1 0.2 0.01
>       ; rate <- dseq dinf (mce2 w b)
>       ; q <- dseq dinf (mce [1..10])
>       ; g1 <- grayNoise AR
>       ; g2 <- grayNoise AR
>       ; d <- lfdNoise3 KR 0.5
>       ; let { tr = trig (tDuty KR rate 0 DoNothing q) 0.09
>             ; pat = latch tr tr
>             ; x = mouseX KR 0 1 Linear 0.2
>             ; h = hasher (pat * x)
>             ; which = trunc (range h 0 (constant (length numbers))) 1
>             ; both = select which (mce_r numbers)
>             ; dial = select both (mce_r (transpose mfv))
>             ; sig = sinOsc AR dial 0 * 0.05 * tr
>             ; dsig = delayN sig 0.2 (range d 0 0.01)
>             ; hiss = g1 * 0.01 + hpf (g2 * 0.02) 3000
>             ; z = silent 1 }
>         in audition (out 0 (mce2 z (dsig + hiss))) }

{ var mfv = [[697, 770, 852, 941], [1209, 1336, 1477, 1633]]
; var numbers = [[3, 1]] ++ {: [a, b], a <- (0..2), b <- (0..2) }.all
; var n = Dwhite(7, 12, inf)
; var w = Dwhite(2, 7, 1)
; var b = Dbrown(0.1, 0.2, 0.01, n)
; var rate = Dseq([w, b], inf)
; var q = Dseq((1..10), inf)
; var trig = Trig.kr(TDuty.kr(rate, 0, q), 0.09)
; var pat = Latch.kr(trig, trig)
; var x = MouseX.kr(0, 1, 'linear', 0.2)
; var h = Hasher.kr(pat * x)
; var which = h.range(0, numbers.size).trunc.(1)
; var both = Select.kr(which, numbers)
; var dial = Select.kr(both, mfv.flop)
; var sig = SinOsc.ar(dial, 0) * 0.05 * trig
; var d = LFDNoise3.kr(0.5)
; var dsig = DelayC.ar(sig, 0.2, d.range(0, 0.01))
; var g1 = GrayNoise.ar
; var g2 = GrayNoise.ar
; var z = Silent.ar(1)
; var hiss = (g1 * 0.01) + HPF.ar(g2 * 0.02, 3000)
; Out.ar(0, [z, dsig + hiss]) }.play
