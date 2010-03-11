what was i thinking? (jmcc)

> import Sound.SC3.Monadic

> main =
>   do { n0 <- lfNoise1 kr 0.2
>      ; n1 <- lfNoise1 kr 0.157
>      ; let { p = pulse ar f (n1 * 0.4 + 0.5) * 0.04
>            ; i = lfPulse ar 0.1 0 0.05 * impulse ar 8 0 * 500
>            ; d = decay i 2
>            ; f = max (sinOsc kr 4 0 + 80) d
>            ; z = rlpf p (n0 * 2000 + 2400) 0.2
>            ; c x = do { r <- rand 0 0.3
>                       ; n <- lfNoise1 kr r
>                       ; return (combL x 0.06 (n * 0.025 + 0.035) 1) }
>            ; y = z * 0.6 }
>        in do { z0 <- clone 2 (c y)
>              ; z1 <- clone 2 (c y)
>              ; audition (out 0 (z + mce [mix z0, mix z1])) } }

{ var n0 = LFNoise1.kr(0.2, 2000, 2400)
; var n1 = LFNoise1.kr(0.157, 0.4, 0.5)
; var i = LFPulse.ar(0.1, 0, 0.05) * Impulse.ar(8, 0) * 500
; var d = Decay.ar(i, 2)
; var f = max(SinOsc.kr(4, 0) + 80, d)
; var p = Pulse.ar(f, n1) * 0.04
; var z = RLPF.ar(p, n0, 0.2)
; var c = { arg i
          ; var r = Rand.new(0, 0.3)
          ; var n = LFNoise1.kr(r, 0.025, 0.035)
          ; CombL.ar(i, 0.06, n, 1) }
; var y = z * 0.6
; Out.ar(0, z + [ c.value(y) + c.value(y)
                , c.value(y) + c.value(y) ]) }.play
