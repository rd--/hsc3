-- pv_BinBufRd ; play analysis back ; c.f. pv_RecordBuf help file
let snd_buf = control kr "buf" 100
    rec_buf = control kr "rec" 10
    fft_buf = localBufId 'α' 1 1024
    x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 5 100 Linear 0.2
    c0 = fft' fft_buf (playBuf 1 ar snd_buf (bufRateScale kr snd_buf) 1 0 Loop DoNothing)
    c1 = X.pv_BinBufRd c0 rec_buf x 10 3 y 1
in ifft c1 1 0
