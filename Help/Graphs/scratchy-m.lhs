scratchy-m (jmcc)

> import Sound.SC3.Monadic

> main =
>   do { n <- clone 2 (brownNoise ar)
>      ; let f = max (n * 0.5 - 0.49) 0 * 20
>        in audition (out 0 (rhpf f 5000 1)) }

{ var n = BrownNoise.ar([0.5, 0.5])
; var f = (n - 0.49).max(0) * 20
; Out.ar(0, RHPF.ar(f, 5000, 1)) }.play
