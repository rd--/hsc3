brf in freq rq

Second order Butterworth band reject filter.

> let f = fsinosc KR (xline KR 0.7 300 20 2) 0 * 3800 + 4000
> in brf (saw AR 200 * 0.1) f 0.3
