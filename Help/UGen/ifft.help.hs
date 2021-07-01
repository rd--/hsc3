-- ifft ; no modification
let z = whiteNoiseId 'α' ar
    c = fft (localBufId 'β' 1 2048) z 0.5 0 1 0
in ifft c 0 0 * 0.05

-- ifft ; variants with the default parameters
let z = whiteNoiseId 'α' ar
    c = fft' (localBufId 'β' 1 2048) z
in ifft' c * 0.05
