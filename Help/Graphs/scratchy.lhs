scratchy (jmcc)

> import Sound.SC3
> import qualified Sound.SC3.UGen.Base as B

> main =
>   do { n <- clone 2 (brownNoise ar)
>      ; let f = max (n * 0.5 - 0.49) 0 * 20
>        in audition (out 0 (rhpf f 5000 1)) }

{ var n = BrownNoise.ar([0.5, 0.5])
; var f = (n - 0.49).max(0) * 20
; Out.ar(0, RHPF.ar(f, 5000, 1)) }.play

the same graph, written using a non-monadic noise constructor

> alternate =
>   let { f m = B.brownNoise m ar * 0.5 - 0.49
>       ; n = mce [f 'a', f 'b'] }
>   in audition (out 0 (rhpf (max n 0 * 20) 5000 1))
