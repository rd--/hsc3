fos in a0 a1 b1

First order filter section.

Same as OnePole.

> let x = lfTri AR 0.4 0 * 0.99
> in audition (out 0 (fos (lfSaw AR 200 0 * 0.2) (1 - (abs x)) 0 x))

Same as OneZero

> let x = lfTri AR 0.4 0 * 0.99
> in audition (out 0 (fos (lfSaw AR 200 0 * 0.2) (1 - (abs x)) x 0))
