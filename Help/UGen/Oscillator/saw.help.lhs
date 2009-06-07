saw AR freq

Band limited sawtooth wave generator.

> import Sound.SC3

> audition (out 0 (saw AR (xLine KR 40 4000 6 RemoveSynth) * 0.1))

Two band limited sawtooth waves thru a resonant low pass filter

> let f = xLine KR 8000 400 5 DoNothing
> in audition (out 0 (rlpf (saw AR (mce [100, 250]) * 0.1) f 0.05))
