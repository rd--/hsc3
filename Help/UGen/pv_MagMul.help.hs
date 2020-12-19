let z = soundIn 0
    y = lfSaw AR (midiCPS 43) 0 * 0.2
    c0 = fft' (localBuf 'α' 2048 1) y
    c1 = fft' (localBuf 'β' 2048 1) z
    c2 = pv_MagMul c0 c1
in ifft' c2 * 0.1
