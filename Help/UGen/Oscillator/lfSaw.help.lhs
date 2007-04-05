lfSaw rate freq iphase

Sawtooth oscillator.  A non-band-limited sawtooth
oscillator. Output ranges from -1 to +1.

freq   - frequency in Hertz
iphase - phase in radians

> audition (out 0 (lfSaw AR 500 1 * 0.1))

Used as both Oscillator and LFO.

> audition (out 0 (lfSaw AR (lfSaw KR 4 0 * 400 + 400) 0 * 0.1))
