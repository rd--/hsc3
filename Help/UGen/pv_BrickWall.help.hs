-- pv_BrickWall
let z = soundIn 0
    x = mouseX KR (-1) 1 Linear 0.1
    c = fft' (localBuf 'α' 2048 1) z
in ifft' (pv_BrickWall c x)
