impulse freq iphase

Impulse oscillator.  Outputs non band limited single sample
impulses.

freq - frequency in Hertz
phase - phase offset in cycles (0..1)

> impulse AR 800 0 * 0.1

> impulse AR (xline KR 800 10 5 2) 0.0 * 0.1

> impulse AR (mousey KR 4 8) (MCE [0, mousex KR 0 1]) * 0.1
