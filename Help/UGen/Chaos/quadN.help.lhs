quadN rate freq a b c xi
quadL rate freq a b c xi
quadC rate freq a b c xi

freq    - iteration frequency in Hertz
a, b, c - equation variables
xi      - initial value of x

General quadratic map chaotic generator.  Non-, linear- and cubic-
interpolating sound generators based on the difference equation:
xn+1 = axn2 + bxn + c

> import Sound.SC3

> audition (out 0 (quadC AR 4000 1 (-1) (-0.75) 0 * 0.2))

> let x = mouseX KR 3.5441 4 Linear 0.1
> in audition (out 0 (quadC AR 4000 (negate x) x 0 0.1 * 0.4))

> let { x = mouseX KR 3.5441 4 Linear 0.1
>     ; f = quadC AR 4 (negate x) x 0 0.1 * 800 + 900 }
> in audition (out 0 (sinOsc AR f 0 * 0.4))
