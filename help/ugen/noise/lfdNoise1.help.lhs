lfdnoise1 id freq

Dynamic ramp noise. Like LFNoise1, it generates linearly
interpolated random values at a rate given by the freq argument,
with two differences: no time quantization, and fast recovery from
low freq values.

LFNoise0,1,2 quantize to the nearest integer division of the
samplerate, and they poll the freq argument only when scheduled,
and thus seem to hang when freqs get very low.

If you don't need very high or very low freqs, or use fixed freqs,
LFNoise1 is more efficient.
