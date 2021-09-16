let z = soundIn 0
    y = lfSaw ar (midiCps 43) 0 * 0.2
    c0 = fft' (localBufId 'α' 2048 1) y
    c1 = fft' (localBufId 'β' 2048 1) z
    c2 = pv_MagMul c0 c1
in ifft' c2 * 0.1
