median length in

Median filter.

Signal with impulse noise.

> do { n <- dust2 AR 100
>    ; audition (out 0 (median 3 (saw AR 500 * 0.1 + n * 0.9))) }

The median length can be increased for longer duration noise.

> do { n <- dust2 AR 100
>    ; audition (out 0 (median 5 (saw AR 500 * 0.1 + lpz1 (n * 0.9)))) }

Long Median filters begin chopping off the peaks of the waveform

> let x = sinOsc AR 1000 0 * 0.2
> in audition (out 0 (mce [x, median 31 x]))

Another noise reduction application. Use Median filter for high
frequency noise.  Use LeakDC for low frequency noise.

> do { n <- whiteNoise AR
>    ; let s = median 31 (n * 0.1 + sinOsc AR 800 0 * 0.1)
>      in audition (out 0 (leakDC s 0.9)) }
