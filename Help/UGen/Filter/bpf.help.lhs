bpf in freq rq

Second order Butterworth bandpass filter

in    - input signal to be processed
freq  - cutoff frequency in Hertz
rq    - the reciprocal of Q, ie. bandwidth / cutoffFreq

> let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
> audition (out 0 (bpf (saw AR 200 * 0.5) f 0.3 ))

> n <- whiteNoise AR
> let x = mouseX KR 220 440 Exponential 0.1
> let y = mouseY KR 0 0.01 Linear 0.1
> audition (out 0 (bpf n (MCE [x, 550 - x]) y))
