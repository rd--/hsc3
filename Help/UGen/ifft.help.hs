-- ifft ; no modification
let z = whiteNoise 'α' AR
    c = fft (localBuf 'β' 1 2048) z 0.5 0 1 0
in ifft c 0 0 * 0.05

-- ifft ; variants with the default parameters
let z = whiteNoise 'α' AR
    c = fft' (localBuf 'β' 1 2048) z
in ifft' c * 0.05