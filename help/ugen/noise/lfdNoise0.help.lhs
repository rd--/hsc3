lfdnoise0 id freq

Dynamic step noise. Like LFNoise0, it generates random values at a
rate given by the freq argument, with two differences: no time
quantization, and fast recovery from low freq values.

LFNoise0,1,2 quantize to the nearest integer division of the
samplerate, and they poll the freq argument only when scheduled,
and thus seem to hang when freqs get very low.

If you don't need very high or very low freqs, or use fixed freqs,
LFNoise0 is more efficient.

Try wiggling mouse quickly; LFNoise frequently seems stuck,
LFDNoise changes smoothly.

> lfnoise0 0 AR (mousex KR 0.1 1000 1 0.2) * 0.1

> lfdnoise0 0 AR (mousex KR 0.1 1000 1 0.2) * 0.1

silent for 2 secs before going up in freq

> lfnoise0 0 AR (xline KR 0.5 10000 3 2) * 0.1

> lfdnoise0 0 AR (xline KR 0.5 10000 3 2) * 0.1

LFNoise quantizes time steps at high freqs, LFDNoise does not:

> lfnoise0 0 AR (xline KR 1000 20000 10 2) * 0.1

> lfdnoise0 0 AR (xline KR 1000 20000 10 2) * 0.1

