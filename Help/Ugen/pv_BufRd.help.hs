-- pv_BufRd ; play analysis back ; c.f. pv_RecordBuf help file
let rec_buf = control kr "rec" 10
    fft_buf = localBufId 'α' 1 1024
    x = mouseX kr 0 1 Linear 0.2
    c0 = X.pv_BufRd fft_buf rec_buf x
in ifft c0 1 0
