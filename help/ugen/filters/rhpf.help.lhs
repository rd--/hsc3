rhpf in freq rq

A resonant high pass filter.

> let f = fsinosc KR (xline KR 0.7 300 20 2) 0 * 3600 + 4000
> in rhpf AR (saw AR 200 * 0.1) f 0.2
