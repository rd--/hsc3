-- pv_HainsworthFoote
let i = soundIn 0
    b = localBuf 'α' 2048 1
    f = fft' b i
    x = mouseX kr 0.5 1.25 Linear 0.2
    h = pv_HainsworthFoote f 1 0 x 0.04
    o = sinOsc ar (mrg2 440 445) 0 * decay (h * 0.1) 0.1 * 0.1
in o + i

-- pv_HainsworthFoote ; spot note transitions
let s = lfSaw ar (lfNoise0 'β' kr 1 * 90 + 400) 0 * 0.5
    b = localBuf 'γ' 2048 1
    f = fft' b s
    d = pv_HainsworthFoote f 1.0 0.0 0.9 0.5
    t = sinOsc ar 440 0 * decay (d * 0.1) 0.1 * 0.1
in mce2 (s * 0.05) t
