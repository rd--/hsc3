mouseX KR minval maxval warp lag

Report mouse location on root window of the machine that the synthesis
server is running on.

> import Sound.SC3

> let x = mouseX KR 40 10000 Exponential 0.2
> in audition (out 0 (sinOsc AR x 0 * 0.1))

There is a variant that takes the same arguments but is a random
traversal.

> let x = mouseX' KR 40 10000 Exponential 0.2
> in audition (out 0 (sinOsc AR x 0 * 0.1))
