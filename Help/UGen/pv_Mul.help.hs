-- pv_Mul
let o1 = sinOsc ar 500 0
    o2 = sinOsc ar (line kr 50 400 5 RemoveSynth) 0
    c1 = fft' (localBufId 'α' 2048 1) o1
    c2 = fft' (localBufId 'β' 2048 1) o2
    h = pv_Mul c1 c2
in ifft' h * 0.1
