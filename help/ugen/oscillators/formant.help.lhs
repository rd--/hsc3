formant fundfreq formfreq bwfreq

Formant oscillator. Generates a set of harmonics around a formant
frequency at a given fundamental frequency.

Modulate fundamental frequency, formant frequency stays constant.

> formant AR (xline KR 400 1000 8 2) 2000 800 * 0.125

Modulate formant frequency, fundamental frequency stays constant.

> formant AR f (xline KR 400 4000 8 2) 200 * 0.125
>     where f = MCE [200, 300, 400, 500]

Modulate width frequency, other frequencies stay constant.

> formant AR 400 2000 (xline KR 800 8000 8 2) * 0.1
