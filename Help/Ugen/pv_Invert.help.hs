-- pv_Invert
let z = sinOsc ar 440 0 * 0.4 + pinkNoiseId 'α' ar * 0.1
    c0 = fft' (localBufId 'β' 2048 1) z
    c1 = X.pv_Invert c0
in mce2 z (ifft' c1) * 0.5

-- pv_Invert
let z = soundIn 0
    c0 = fft' (localBufId 'β' 2048 1) z
    c1 = X.pv_Invert c0
in mce2 z (ifft' c1) * 0.5
