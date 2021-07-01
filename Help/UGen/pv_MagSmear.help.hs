-- pv_MagSmear
let i = soundIn 0
    c = fft' (localBufId 'α' 2048 1) i
    x = mouseX kr 0 100 Linear 0.2
    h = pv_MagSmear c x
in ifft' h * 0.5
