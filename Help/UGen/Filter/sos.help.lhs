sos in a0 a1 a2 b1 b2

Second order filter section (biquad).  A standard second order
filter section. Filter coefficients are given directly rather than
calculated for you.

Same as TwoPole

> let theta = line KR (0.2 * pi) pi 5 2
>     rho = line KR 0.6 0.99 5 2
>     b1 = 2 * rho * cos theta
>     b2 = - (rho * rho)
> in sos AR (lfsaw AR 200 * 0.1) 1 0 0 b1 b2
