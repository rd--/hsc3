scratchy 

> do { n <- clone 2 (brownNoise AR)
>    ; let f = max (n * 0.5 - 0.49) 0 * 20
>      in audition (out 0 (rhpf f 5000 1)) }

{ var n = BrownNoise.ar([0.5, 0.5])
; var f = (n - 0.49).max(0) * 20
; Out.ar(0, RHPF.ar(f, 5000, 1)) }.play

with non-monadic noise

> let { bn = Sound.SC3.UGen.Noise.Base.brownNoise
>     ; f m = bn (UGenId m) AR * 0.5 - 0.49
>     ; n = mce [f 0, f 1] }
> in audition (out 0 (rhpf (max n 0 * 20) 5000 1))
