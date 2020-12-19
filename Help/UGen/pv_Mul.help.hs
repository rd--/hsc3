-- pv_Mul
let o1 = sinOsc AR 500 0
    o2 = sinOsc AR (line KR 50 400 5 RemoveSynth) 0
    c1 = fft' (localBuf 'α' 2048 1) o1
    c2 = fft' (localBuf 'β' 2048 1) o2
    h = pv_Mul c1 c2
in ifft' h * 0.1
