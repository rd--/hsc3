scratchy (jmcc)

> do { n <- clone 2 (brownNoise AR)
>    ; let f = max (n * 0.5 - 0.49) 0 * 20
>      in audition (out 0 (rhpf f 5000 1)) }

{ var n = BrownNoise.ar([0.5, 0.5])
; var f = (n - 0.49).max(0) * 20
; Out.ar(0, RHPF.ar(f, 5000, 1)) }.play

(let* ((n (Mul (clone 2 (BrownNoise ar)) 0.5))
       (f (Mul (Max (Sub n 0.49) 0) 20)))
  (audition (Out 0 (RHPF f 5000 1))))

with non-monadic noise

> let { bn = Sound.SC3.UGen.Base.brownNoise
>     ; f m = bn (uid m) AR * 0.5 - 0.49
>     ; n = mce [f 0, f 1] }
> in audition (out 0 (rhpf (max n 0 * 20) 5000 1))
