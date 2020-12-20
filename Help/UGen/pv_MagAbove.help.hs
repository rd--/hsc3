-- pv_MagAbove
let z = soundIn 0
    f = fft' (localBuf 'α' 2048 1) z
    x = mouseX KR 0 64 Linear 0.1
    h = pv_MagAbove f x
in ifft' h * 0.5

-- pv_MagAbove ; synthesised input
let z = sinOsc AR (sinOsc KR (squared (sinOsc KR 0.08 0 * 6 + 6.2)) 0 * 100 + 800) 0
    f = fft' (localBuf 'α' 2048 1) z
    x = mouseX KR 0 512 Linear 0.1
    h = pv_MagAbove f x
in ifft' h * 0.1
