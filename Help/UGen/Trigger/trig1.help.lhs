trig1 in dur

When `in' is trigered output a unit signal for `dur' seconds.

> d <- dust AR 1
> audition $ trig1 d 0.2 * fSinOsc AR 800 0 * 0.2
