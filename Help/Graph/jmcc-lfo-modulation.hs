-- lfo modulation (jmcc) #1
let o = fSinOsc KR 0.05 0 * 80 + 160
    p = fSinOsc KR (mce2 0.6 0.7) 0 * 3600 + 4000
    s = rlpf (lfPulse AR o 0 0.4 * 0.05) p 0.2
in combL s 0.3 (mce2 0.2 0.25) 2
