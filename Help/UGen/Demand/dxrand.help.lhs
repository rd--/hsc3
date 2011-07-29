See drand.

> import Sound.SC3.ID

> let { i = mce [0.2,0.4,dseq 'a' 2 (mce [0.1,0.1])]
>     ; d = dxrand 'b' dinf i
>     ; t = tDuty AR d 0 DoNothing (dwhite 'c' dinf 0.5 1) 0 }
> in audition (out 0 t) >> Sound.SC3.UGen.Dot.draw t

Demand rate binary math is not working properly?  Below the i' variant
is correct, the i'' variant incorrect.  In the first case the
constants are re-written, in the second they introduce a deman rate
binop with two constant inputs which seems to make an infinite
sequence of the value.

> let { i = mce [0.2,0.4,dseq 'a' 2 (mce [0.1,0.1])]
>     ; i' = mceMap (* 0.5) i
>     ; i'' = i * 0.5
>     ; d = dxrand 'b' dinf i'
>     ; t = tDuty AR d 0 DoNothing (dwhite 'c' dinf 0.5 1) 0 }
> in audition (out 0 t) >> Sound.SC3.UGen.Dot.draw t

> let { n = dxrand 'a' dinf (mce [1, 3, 2, 7, 8])
>     ; x = linExp (lfNoise0 'b' KR 1) (-1) 1 1 400
>  {- ; x = mouseX KR 1 400 Exponential 0.1 -}
>     ; t = impulse KR x 0
>     ; f = demand t 0 n * 30 + 340 }
> in audition (out 0 (sinOsc AR f 0 * 0.1))
