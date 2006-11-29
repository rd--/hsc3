trig in dur

When `in' is trigerred output the trigger value for `dur' seconds.

> d <- dust AR 1
> audition $ trig d 0.2 * fSinOsc AR 800 0 * 0.5
