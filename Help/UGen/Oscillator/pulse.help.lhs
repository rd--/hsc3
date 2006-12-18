pulse rate freq width

Bandlimited pulse wave generator.

Modulate frequency

> audition $ pulse AR (xLine KR 40 4000 6 RemoveSynth) 0.1 * 0.1

Modulate pulse width

> audition $ pulse AR 200 (line KR 0.01 0.99 8 RemoveSynth) * 0.1

Two band limited square waves thru a resonant low pass filter

> let p = pulse AR (MCE [100, 250]) 0.5 * 0.1
> audition $ rlpf p (xLine KR 8000 400 5 RemoveSynth) 0.05
