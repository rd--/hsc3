pulse AR f w=0.5

   r - operating rate
   f - frequency (hz)
   w - width (0.5)

Bandlimited pulse wave generator.

Modulate frequency

> import Sound.SC3

> let f = xLine KR 40 4000 6 RemoveSynth
> in audition (out 0 (pulse AR f 0.1 * 0.1))

Modulate pulse width

> let w = line KR 0.01 0.99 8 RemoveSynth
> in audition (out 0 (pulse AR 200 w * 0.1))

Two band limited square waves through a resonant 
low pass filter

> let { p = pulse AR (mce [100, 250]) 0.5 * 0.1
>     ; f = xLine KR 8000 400 5 RemoveSynth }
> in audition (out 0 (rlpf p f 0.05))
