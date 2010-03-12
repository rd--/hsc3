offsetOut bufferIndex inputs
 
Output signal to a bus, the sample offset within the bus is kept
exactly.  This ugen is used where sample accurate output is needed.

> import Sound.SC3

> let { a = offsetOut 0 (impulse AR 5 0)
>     ; b = out 0 (sinOsc AR 60 0 * 0.1) }
> in audition (mrg [a, b])

> let { a = out 0 (impulse AR 5 0)
>     ; b = out 0 (sinOsc AR 60 0 * 0.1) }
> in audition (mrg [a, b])
