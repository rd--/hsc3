sinOsc rate freq phase

Interpolating sine wavetable oscillator.  This is the same as osc
except that the table is a sine table of 8192 entries.

freq  - frequency in Hertz
phase - phase offset or modulator in radians

> audition (out 0 (sinOsc AR 440 0 * 0.25))

Modulate freq

> audition (out 0 (sinOsc AR (xLine KR 2000 200 9 RemoveSynth) 0 * 0.5))

Modulate freq

> let f = sinOsc AR (xLine KR 1 1000 9 RemoveSynth) 0 * 200 + 800
> in audition (out 0 (sinOsc AR f 0 * 0.1))

Modulate phase

> let p = sinOsc AR (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
> in audition (out 0 (sinOsc AR 800 p * 0.1))
