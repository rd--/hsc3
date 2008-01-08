select which array

The output is selected from an array of inputs.

> let n = 3/2
>     a = mce [sinOsc AR 440 0, saw AR 440, pulse AR 440 0.1]
> audition (out 0 (select (lfSaw KR 1 0 * n + n) a * 0.2))

Note: all input ugens are continously running. This may not be the
most efficient way if each input is cpu-expensive.

Here used as a sequencer:

> let n = 10
>     a = mce [517, 403, 89, 562, 816, 107, 241, 145, 90, 224]
>     c = n / 2
> audition (out 0 (saw AR (select (lfSaw KR 0.5 0 * c + c) a) * 0.2))
