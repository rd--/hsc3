pulse freq width

Bandlimited pulse wave generator.

Modulate frequency

> pulse AR (xline KR 40 4000 6 2) 0.1 * 0.1

Modulate pulse width

> pulse AR 200 (line KR 0.01 0.99 8 2) * 0.1

Two band limited square waves thru a resonant low pass filter

> rlpf AR (pulse AR (MCE [100, 250]) 0.5 * 0.1) (xline KR 8000 400 5 2) 0.05
