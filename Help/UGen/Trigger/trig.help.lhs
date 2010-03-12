trig in dur

When `in' is trigerred output the trigger value for `dur' seconds.

> import Sound.SC3.ID

> let { d = dust 'a' AR 1
>     ; o = fSinOsc AR 800 0 * 0.5 }
> in audition (out 0 (trig d 0.2 * o))
