-- pv_MagGate
let (lhs,rhs) = (-50,0) -- (0,100)
    i = soundIn 0
    c = fft' (localBuf 'α' 2048 1) i
    x = mouseX KR lhs rhs Linear 0.2
    y = mouseY KR 0 1 Linear 0.2
    h = X.pv_MagGate c x y
in ifft' h * 0.5
