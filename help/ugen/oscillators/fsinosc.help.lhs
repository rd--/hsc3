fsinosc freq iphase

Very fast sine wave generator implemented using a ringing filter.
This generates a much cleaner sine wave than a table lookup
oscillator and is a lot faster.  However, the amplitude of the wave
will vary with frequency. Generally the amplitude will go down as
you raise the frequency and go up as you lower the frequency.

WARNING: In the current implementation, the amplitude can blow up
if the frequency is modulated by certain alternating signals.

freq - frequency in Hertz
iphase - initial phase

Note this has an extra iphase argument from the SC2 variant.

> fsinosc AR '(440 550) 0 0.05

> fsinosc AR (xline KR 200 4000 1) 0 0.25

Loses amplitude towards the end

> fsinosc AR (fsinosc AR (xline KR 4 401 8) 0 200 800) 0 0.25
