-- pv_MagFreeze ; mouseX is trigger to freeze
let b = control kr "buf" 0
    z = playBuf 1 ar b (bufRateScale kr b) 0 0 Loop DoNothing
    f = fft' (localBufId 'α' 2048 1) z
    x = mouseX kr 0 1 Linear 0.1
    h = pv_MagFreeze f (x >** 0.5)
in ifft' h * 0.5

-- pv_MagFreeze ; mouseX is trigger to freeze
let z = soundIn 0
    f = fft' (localBufId 'α' 2048 1) z
    x = mouseX kr 0 1 Linear 0.1
    h = pv_MagFreeze f (x >** 0.5)
in ifft' h * 0.5
