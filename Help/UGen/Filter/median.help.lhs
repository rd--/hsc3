median length in

Median filter.

Signal with impulse noise.

> median AR 3 (saw AR 500 * 0.1 + dust2 0 AR 100 * 0.9)

The median length can be increased for longer duration noise.

> median AR 5 (saw AR 500 * 0.1 + lpz1 AR (dust2 0 AR 100 * 0.9))

Long Median filters begin chopping off the peaks of the waveform

> let x = sinosc AR 1000 0 * 0.2
> in MCE [x, median AR 31 x]

Another noise reduction application. Use Median filter for high
frequency noise.  Use LeakDC for low frequency noise.

> leakdc AR (median AR 31 (whitenoise 0 AR * 0.1 + sinosc AR 800 0 * 0.1)) 0.9
