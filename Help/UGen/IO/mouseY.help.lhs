mouseY KR minval maxval warp lag

Report mouse location on root window of the machine that the
synthesis server is running on.

> import Sound.SC3

> let { freq = mouseX KR 20 2000 Exponential 0.1
>     ; ampl = mouseY KR 0.01 0.1 Linear 0.1 }
> in audition (out 0 (sinOsc AR freq 0 * ampl))

There is a variant that takes the same arguments but is a random
traversal.

> let { freq = mouseX' KR 20 2000 Exponential 0.1
>     ; ampl = mouseY' KR 0.01 0.1 Linear 0.1 }
> in audition (out 0 (sinOsc AR freq 0 * ampl))
