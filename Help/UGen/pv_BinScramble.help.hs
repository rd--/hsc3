-- pv_BinScramble
let b = control KR "buf" 0
    a = playBuf 1 AR b (bufRateScale KR b) 1 0 Loop DoNothing
    f = fft' (localBuf 'α' 2048 1) a
    x = mouseX KR 0.0 1.0 Linear 0.1
    y = mouseY KR 0.0 1.0 Linear 0.1
    g = pv_BinScramble 'β' f x y (impulse KR 4 0)
in pan2 (ifft' g) 0 0.5

-- pv_BinScramble ; warning=feedback
let a = soundIn 0
    f = fft' (localBuf 'γ' 2048 1) a
    x = mouseX KR 0.15 1 Linear 0.1
    y = mouseY KR 0.15 1 Linear 0.1
    i = impulse KR (lfNoise0 'δ' KR 2 * 8 + 10) 0
    g = pv_BinScramble 'ε' f x y i
    h = ifft' g
in pan2 h 0 1

---- ; load sndfile
withSC3 (async (b_allocRead 0 "/home/rohan/data/audio/pf-c5.aif" 0 0))
