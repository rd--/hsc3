impulse freq iPhase

Impulse oscillator.  Outputs non band limited single sample impulses.

freq  - frequency in Hertz
phase - phase offset in cycles (0..1)

> audition $ impulse AR 800 0 * 0.1

> audition $ impulse AR (xLine KR 800 10 5 RemoveSynth) 0.0 * 0.1

> audition $ impulse AR (mouseY KR 4 8 0 0.1) (MCE [0, mouseX KR 0 1 0 0.1]) * 0.1
