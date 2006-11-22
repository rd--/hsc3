sinosc freq phase

Interpolating sine wavetable oscillator.  This is the same as osc
except that the table is a sine table of 8192 entries.

freq - frequency in Hertz
phase - phase offset or modulator in radians

> sinosc AR 440 0 * 0.5

Modulate freq

> sinosc AR (xline KR 2000 200 2) 0 * 0.5

Modulate freq

> sinosc AR (sinosc AR (xline KR 1 1000 9 2) 0 * 200 + 800) 0 * 0.1

Modulate phase

> sinosc AR 800 (sinosc AR (xline KR 20 8000 10 2) 0 * (2 * pi)) * 0.1
