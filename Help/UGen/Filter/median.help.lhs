> Sound.SC3.UGen.Help.viewSC3Help "Median"
> Sound.SC3.UGen.DB.ugenSummary "Median"

> import Sound.SC3.ID

Signal with impulse noise.
> let n = dust2 'a' AR 100
> in audition (out 0 (median 3 (saw AR 500 * 0.1 + n * 0.9)))

The median length can be increased for longer duration noise.
> let n = dust2 'a' AR 100
> in audition (out 0 (median 5 (saw AR 500 * 0.1 + lpz1 (n * 0.9))))

Long Median filters begin chopping off the peaks of the waveform
> let x = sinOsc AR 1000 0 * 0.2
> in audition (out 0 (mce [x, median 31 x]))

Another noise reduction application. Use Median filter for high
frequency noise.  Use LeakDC for low frequency noise.
> let {n = whiteNoise 'a' AR
>     ;s = median 31 (n * 0.1 + sinOsc AR 800 0 * 0.1)}
> in audition (out 0 (leakDC s 0.9))
