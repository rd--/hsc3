stkBowed rt freq bowPressure bowPosition vibFreq vibGain loudness gate

> import Sound.SC3

> let g = toggleFF (impulse KR 1 0)
> in audition (out 0 (stkBowed AR 220 64 64 64 64 64 g))
