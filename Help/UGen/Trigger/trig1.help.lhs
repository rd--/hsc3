trig1 in dur

When `in' is trigered output a unit signal for `dur' seconds.

> import Sound.SC3.ID

> let d = dust 'a' AR 1
> in audition (out 0 (trig1 d 0.2 * fSinOsc AR 800 0 * 0.2))
