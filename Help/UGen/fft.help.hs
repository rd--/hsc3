-- fft ; variant with default values
let n = whiteNoiseId 'α' ar
in ifft' (fft' (localBufId 'β' 1 2048) (n * 0.05))

-- fft ; local buffer allocating variant
let s0 = sinOsc kr 0.08 0 * 6 + 6.2
    s1 = sinOsc kr (squared s0) 0 * 100 + 800
    s2 = sinOsc ar s1 0
in ifft (fftAllocId 'α' 2048 s2 0.5 0 1 0) 0 0 * 0.1
