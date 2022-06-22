-- pv_BinScramble
let b = control kr "buf" 0
    a = playBuf 1 ar b (bufRateScale kr b) 1 0 Loop DoNothing
    f = fft' (localBufId 'α' 2048 1) a
    x = mouseX kr 0.0 1.0 Linear 0.1
    y = mouseY kr 0.0 1.0 Linear 0.1
    g = pv_BinScrambleId 'β' f x y (impulse kr 4 0)
in pan2 (ifft' g) 0 0.5

-- pv_BinScramble ; warning=feedback
let a = soundIn 0
    f = fft' (localBufId 'γ' 2048 1) a
    x = mouseX kr 0.15 1 Linear 0.1
    y = mouseY kr 0.15 1 Linear 0.1
    i = impulse kr (lfNoise0Id 'δ' kr 2 * 8 + 10) 0
    g = pv_BinScrambleId 'ε' f x y i
    h = ifft' g
in pan2 h 0 1

---- ; load sndfile
withSc3 (async (b_allocRead 0 "/home/rohan/data/audio/pf-c5.aif" 0 0))
