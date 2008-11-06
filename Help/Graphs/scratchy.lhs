scratchy (jmcc)

> import Sound.SC3

> do { n <- clone 2 (brownNoise ar)
>    ; let f = max (n * 0.5 - 0.49) 0 * 20
>      in audition (out 0 (rhpf f 5000 1)) }

{ var n = BrownNoise.ar([0.5, 0.5])
; var f = (n - 0.49).max(0) * 20
; Out.ar(0, RHPF.ar(f, 5000, 1)) }.play

the same graph, written using a non-monadic noise constructor

> import qualified Sound.SC3.UGen.Base as B

> let { f m = B.brownNoise (uid m) ar * 0.5 - 0.49
>     ; n = mce [f 0, f 1] }
> in audition (out 0 (rhpf (max n 0 * 20) 5000 1))
