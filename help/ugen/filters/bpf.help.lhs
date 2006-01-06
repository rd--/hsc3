bpf in freq rq

Second order Butterworth bandpass filter

in    - input signal to be processed
freq  - cutoff frequency in Hertz
rq    - the reciprocal of Q, ie. bandwidth / cutoffFreq

> let f = fsinosc KR (xline KR 0.7 300 20 2) 0 * 3600 + 4000
> in bpf (saw AR 200 * 0.5) f 0.3

> let f = mousex KR 220 440
> in bpf (whitenoise 0 AR) (MCE [f, 550 - f]) (mousey KR 0 0.01)
