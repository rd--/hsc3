latoocarfianC rate freq a b c d xi yi
latoocarfianL rate freq a b c d xi yi
latoocarfianN rate freq a b c d xi yi

This is a function given in Clifford Pickover's book Chaos In
Wonderland, pg 26.  The function has four parameters a, b, c, and
d.  The function is:

  xnew = sin(y * b) + c * sin(x * b)
  ynew = sin(x * a) + d * sin(y * a)
  x = xnew
  y = ynew
  output = x

According to Pickover, parameters a and b should be in the range
from -3 to +3, and parameters c and d should be in the range from
0.5 to 1.5.  The function can, depending on the parameters given,
give continuous chaotic output, converge to a single value
(silence) or oscillate in a cycle (tone).  This UGen is
experimental and not optimized currently, so is rather hoggish of
CPU.

sclang default initial parameters.

> let x = mouseX KR 20 sampleRate Linear 0.1
> audition $ latoocarfianC AR x 1 3 0.5 0.5 0.5 0.5 * 0.2

Randomly modulate all parameters.

> [n0, n1, n2, n3] <- replicateM 4 (lfNoise2 KR 2)
> let f = sampleRate / 4
>     a = n0 * 1.5 + 1.5
>     b = n1 * 1.5 + 1.5
>     c = n2 * 0.5 + 1.5
>     d = n2 * 0.5 + 1.5
> audition $ latoocarfianC AR f a b c d 0.5 0.5 * 0.2
