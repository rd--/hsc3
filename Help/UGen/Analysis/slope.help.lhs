slope in

Slope of signal.  Measures the rate of change per second of a
signal.  Formula implemented is:

out[i] = (in[i] - in[i-1]) * sampling_rate

in - input signal to measure.

In the example below a is quadratic noise, b first derivative line
segments, and c second derivative constant segments.

> import Sound.SC3

> do { a <- lfNoise2 KR 2
>    ; let { s = 1/2
>          ; b = slope a * s
>          ; c = slope b * squared s 
>          ; f = mce [a, b, c] * 220 + 220
>          ; o = sinOsc AR f 0 * (1/3) }
>      in audition (out 0 (mix o)) }
