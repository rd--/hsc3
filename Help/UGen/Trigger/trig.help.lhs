trig in dur

When `in' is trigerred output the trigger value for `dur' seconds.

> do { d <- dust AR 1
>    ; let o = fSinOsc AR 800 0 * 0.5
>      in audition (out 0 (trig d 0.2 * o)) }
