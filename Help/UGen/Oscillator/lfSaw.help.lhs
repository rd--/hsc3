lfSaw rate freq iphase

Sawtooth oscillator.  A non-band-limited sawtooth
oscillator. Output ranges from -1 to +1.

freq   - frequency in Hertz
iphase - initial phase [0,2]

> import Sound.SC3

> audition (out 0 (lfSaw AR 500 1 * 0.1))

Used as both Oscillator and LFO.

> audition (out 0 (lfSaw AR (lfSaw KR 4 0 * 400 + 400) 0 * 0.1))

Output range is bi-polar.

> let { f = mce [linLin (lfSaw KR 0.5 0) (-1) 1 200 1600, 200, 1600]
>     ; a = mce [0.1, 0.05, 0.05] }
> in audition (out 0 (mix (sinOsc AR f 0 * a)))
