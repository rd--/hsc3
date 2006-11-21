fSinOsc freq iphase

Very fast sine wave generator implemented using a ringing filter.
This generates a much cleaner sine wave than a table lookup oscillator
and is a lot faster.  However, the amplitude of the wave will vary
with frequency. Generally the amplitude will go down as you raise the
frequency and go up as you lower the frequency.

WARNING: In the current implementation, the amplitude can blow up if
the frequency is modulated by certain alternating signals.

freq   - frequency in Hertz
iphase - initial phase

Note the phase argument, which was not in the SC2 variant.

> fSinOsc AR (MCE [440, 550]) 0 * 0.05

> fSinOsc AR (xLine KR 200 4000 1 RemoveSynth) 0 * 0.1

Loses amplitude towards the end

> fSinOsc AR (fsinosc AR (xLine KR 4 401 8 RemoveSynth) 0 * 200 + 800) 0 * 0.1
