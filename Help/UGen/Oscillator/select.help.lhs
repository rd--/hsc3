select which array

The output is selected from an array of inputs.

> select AR (lfsaw KR 1 0 * cycle + cycle) a * 0.2
>     where a = [sinosc AR 440 0, saw AR 440, pulse AR 440 0.1]
>           cycle = 3.0 / 2.0

Note: all the ugens are continously running. This may not be the
most efficient way if each input is cpu-expensive.

Here used as a sequencer:

> saw AR (select KR (lfsaw KR 0.5 0 * cycle + cycle) a * 0.2
>         where n = 32
>               a = map midicps [...]
>               cycle = n / 2
