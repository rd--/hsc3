-- pv_BinWipe
let z = soundIn 0
    n = whiteNoiseId 'α' ar * 0.1
    f = fft' (localBufId 'β' 2048 1) n
    g = fft' (localBufId 'γ' 2048 1) z
    x = mouseX kr 0.0 1.0 Linear 0.1
    h = pv_BinWipe f g x
in pan2 (ifft' h) 0 0.5
