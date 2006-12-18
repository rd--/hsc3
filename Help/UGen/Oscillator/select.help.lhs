select which array

The output is selected from an array of inputs.

> let a     = MCE [sinOsc AR 440 0, saw AR 440, pulse AR 440 0.1]
>     cycle = 3.0 / 2.0
> audition $ select (lfSaw KR 1 0 * cycle + cycle) a * 0.2

Note: all the ugens are continously running. This may not be the
most efficient way if each input is cpu-expensive.

Here used as a sequencer:

> let n = 12
>     a = MCE [550, 106, 385, 488, 121, 48, 455, 64, 84, 443, 338, 73]
>     cycle = n / 2
> audition $ saw AR (select (lfSaw KR 0.5 0 * cycle + cycle) a) * 0.2
