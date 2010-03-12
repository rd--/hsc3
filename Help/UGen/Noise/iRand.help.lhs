iRand lo hi

Generates a single random integer value in uniform distribution
from `lo' to `hi'.

> import Sound.SC3.ID

> let { f = iRand 'a' 200 1200
>     ; e = line KR 0.2 0 0.1 RemoveSynth }
> in audition (out 0 (fSinOsc AR f 0 * e))
