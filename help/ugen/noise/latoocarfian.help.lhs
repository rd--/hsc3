latoocarfian a b c d

This is a function given in Clifford Pickover's book Chaos In
Wonderland, pg 26.  The function has four parameters a, b, c, and
d.  The function is:

xnew = sin(y * b) + c * sin(x * b);
ynew = sin(x * a) + d * sin(y * a);
x = xnew;
y = ynew;
output = x;

According to Pickover, parameters a and b should be in the range
from -3 to +3, and parameters c and d should be in the range from
0.5 to 1.5.  The function can, depending on the parameters given,
give continuous chaotic output, converge to a single value
(silence) or oscillate in a cycle (tone).  This UGen is
experimental and not optimized currently, so is rather hoggish of
CPU.
