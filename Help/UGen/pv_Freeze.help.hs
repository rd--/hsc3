-- pv_Freeze
let f = fft' (localBufId 'α' 2048 1) (soundIn 0)
    x = mouseX kr 0 1 Linear 0.1
    h = X.pv_Freeze f (x `greater_than` 0.5)
in ifft' h * 0.5
