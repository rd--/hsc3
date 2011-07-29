See drand.

> import Sound.SC3.ID

> let { i = mce [0.2,0.4,dseq 'a' 2 (mce [0.1,0.1])]
>     ; d = dxrand 'b' dinf i
>     ; t = tDuty AR d 0 DoNothing (dwhite 'c' dinf 0.5 1) 0 }
> in audition (out 0 t) >> Sound.SC3.UGen.Dot.draw t

Demand rate and mce do not interact properly.  Below the i' variant is
correct and the i'' variant incorrect.  In the first case the
constants are re-written, in the second they introduce a demand rate
binary operator with two constant inputs, which generates an infinite
sequence of the value.  The problem arises in the mceTransform and
mceExpand implementation, the rate of the filter primitive is set to
the maximum rate of the inputs and is not revised after mce
transformation, where it may be lower.

> let { i = mce [0.2,0.4,dseq 'a' 2 (mce [0.1,0.1])]
>     ; i' = mceMap (* 2) i
>     ; i'' = i * 2
>     ; d = dxrand 'b' dinf i'
>     ; t = tDuty AR d 0 DoNothing (dwhite 'c' dinf 0.5 1) 0 }
> in audition (out 0 t) >> Sound.SC3.UGen.Dot.draw t

> let { n = dxrand 'a' dinf (mce [1, 3, 2, 7, 8])
>     ; x = linExp (lfNoise0 'b' KR 1) (-1) 1 1 400
>  {- ; x = mouseX KR 1 400 Exponential 0.1 -}
>     ; t = impulse KR x 0
>     ; f = demand t 0 n * 30 + 340 }
> in audition (out 0 (sinOsc AR f 0 * 0.1))
