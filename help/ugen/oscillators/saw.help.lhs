saw freq

Band limited sawtooth wave generator.

> saw AR (xline KR 40 4000 6 2) * 0.1

Two band limited sawtooth waves thru a resonant low pass filter

> rlpf AR (saw AR (MCE [100, 250]) * 0.1) (xline KR 8000 400 5 2) 0.05
