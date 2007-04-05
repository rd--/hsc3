linLin in srclo srchi dstlo dsthi

Map a linear range to another linear range.

in    - input to convert.
srclo - lower limit of input range.
srchi - upper limit of input range.
dstlo - lower limit of output range.
dsthi - upper limit of output range.

> let f = linLin (mouseX KR 0 1 Linear 0.2) 0 1 440 660
> audition (out 0 (sinOsc AR f 0 * 0.1))
