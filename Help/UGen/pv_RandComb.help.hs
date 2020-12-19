-- pv_RandComb
let z = soundIn 0
    t = impulse KR 0.5 0
    x = mouseX KR 0.6 0.95 Linear 0.1
    c = pv_RandComb 'α' (fft' (localBuf 'α' 2048 1) z) x t
in pan2 (ifft' c) 0 1
