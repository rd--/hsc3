-- pv_PlayBuf ; c.f. pv_RecordBuf
let rec_buf = control kr "rec" 10
    fft_buf = localBuf 'α' 1024 1
    x = mouseX kr (-1) 1 Linear 0.2
    c = X.pv_PlayBuf fft_buf rec_buf x 50 1
in ifft c 1 0

-- pv_PlayBuf
let rec_buf = control kr "rec" 10
    fft_buf = localBuf 'β' 1024 1
    n = range (-1) 2 (lfNoise2 'γ' kr 0.2)
    c = X.pv_PlayBuf fft_buf rec_buf n 0 1
in ifft c 1 0
