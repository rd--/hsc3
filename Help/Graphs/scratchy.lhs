scratchy (jmcc)

> import Sound.SC3.ID

> main =
>   let { f m = brownNoise m ar * 0.5 - 0.49
>       ; n = mce2 (f 'α') (f 'β') }
>   in audition (out 0 (rhpf (max n 0 * 20) 5000 1))

{ var n = BrownNoise.ar([0.5, 0.5])
; var f = (n - 0.49).max(0) * 20
; Out.ar(0, RHPF.ar(f, 5000, 1)) }.play
