lfdclipnoise id freq

Like LFClipNoise, it generates the values -1 or +1 at a rate given
by the freq argument, with two differences: no time quantization,
and fast recovery from low freq values.

LFClipNoise, as well as LFNoise0,1,2 quantize to the nearest
integer division of the samplerate, and they poll the freq argument
only when scheduled, and thus seem to hang when freqs get very
low.

If you don't need very high or very low freqs, or use fixed freqs,
LFClipNoise is more efficient.

Try wiggling mouse quickly; LFNoise frequently seems stuck,
LFDNoise changes smoothly.

> sinosc AR (lfclipnoise 0 AR (mousex KR 0.1 1000 1 0.2) * 200 + 500) 0 * 0.2

> sinosc AR (lfdclipnoise 0 AR (mousex KR 0.1 1000 1 0.2) * 200 + 500) 0 * 0.2

LFNoise quantizes time steps at high freqs, LFDNoise does not:

> lfclipnoise 0 AR (xline KR 1000 20000 10 2) * 0.1

> lfdclipnoise 0 AR (xline KR 1000 20000 10 2) * 0.1
