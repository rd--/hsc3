a `mod` b

Modulo, written % in sclang.  outputs a modulo b.

> import Sound.SC3

> audition (out 0 (fSinOsc AR 100 4 `mod` 1))
