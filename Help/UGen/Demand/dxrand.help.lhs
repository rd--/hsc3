See drand.

> import Sound.SC3.ID

> let { i = mce [0.2,0.4,dseq 'a' 2 (mce [0.1,0.1])]
>     ; d = dxrand 'b' dinf i
>     ; t = tDuty AR d 0 DoNothing (dwhite 'c' dinf 0.5 1) 0 }
> in audition (out 0 t) >> Sound.SC3.UGen.Dot.draw t

The list inputs to demand rate ugens may operate at different rates.
The variants i' and i'' below ought to generate the same graph.  A
simple-minded mce rule sets the rate of the operator primitive to the
maximum rate of the inputs and then does not revise this after mce
transformation, where it may be lower.  The hsc3 constructors attempt
to get this right!

> let { i = mce [0.2,0.4,dseq 'a' 2 (mce [0.1,0.1])]
>     ; i' = mceMap (* 0.5) i
>     ; i'' = i * 0.5
>     ; d = dxrand 'c' dinf i''
>     ; t = tDuty AR d 0 DoNothing (dwhite 'c' dinf 0.5 1) 0 }
> in audition (out 0 t) >> Sound.SC3.UGen.Dot.draw t

> let { n = dxrand 'a' dinf (mce [1, 3, 2, 7, 8])
>     ; x = mouseX' KR 1 400 Exponential 0.1
>     ; t = impulse KR x 0
>     ; f = demand t 0 n * 30 + 340 }
> in audition (out 0 (sinOsc AR f 0 * 0.1))
