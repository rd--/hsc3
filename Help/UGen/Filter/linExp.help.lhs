linExp in srclo srchi dstlo dsthi

Map a linear range to an exponential range.  The dstlo and dsthi
arguments must be nonzero and have the same sign.

in    - input to convert.
srclo - lower limit of input range.
srchi - upper limit of input range.
dstlo - lower limit of output range.
dsthi - upper limit of output range.

> let f = linExp (mouseX KR 0 1 Linear 0.2) 0 1 440 660
> audition (out 0 (sinOsc AR f 0 * 0.1))

The destination range may be k-rate.

> let x = mouseX KR 0 1 Linear 0.2
>     y = mouseY KR 220 440 Linear 0.2
>     f = linExp x 0 1 y 660
> audition (out 0 (sinOsc AR f 0 * 0.1))
