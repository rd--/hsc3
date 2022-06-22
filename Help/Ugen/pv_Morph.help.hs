-- pv_Morph
let o1 = pulse ar 180 (lfCub kr 1 0 * 0.1 + 0.3) * 0.2
    o2 = varSaw ar 190 0 (lfCub kr 0.8 0 * 0.4 + 0.5) * 0.5
    c1 = fft' (localBufId 'α' 2048 1) o1
    c2 = fft' (localBufId 'β' 2048 1) o2
    x = mouseX kr 0 1 Linear 0.2
    h = X.pv_Morph c1 c2 x
in ifft' h * 0.5
