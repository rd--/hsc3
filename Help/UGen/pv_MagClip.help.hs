-- pv_MagClip
let f = fft' (localBufId 'α' 2048 1) (soundIn 0)
    c = 128
    x = mouseX kr 0 c Linear 0.1
    h = pv_MagClip f x
in ifft' h * 0.5
