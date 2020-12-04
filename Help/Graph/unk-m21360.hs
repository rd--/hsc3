-- https://www.listarc.bham.ac.uk/lists/sc-users/msg21360.html
let k_sr = 48000
    b = mce2 (localBuf 'α' 1 k_sr) (localBuf 'β' 1 k_sr)
    del = mce [0.2,0.3,0.4,0.5] * rand 'γ' 0.15 0.35
    loc = mce [-1,-0.7,0.6,1]
    hpp = mce [rand 'δ' 300 600,rand 'ε' 900 1200]
    avg = mce [8000,14000]
    sdt = mce [300,400]
    lpp = mce [rand 'ζ' 4300 4500,rand 'η' 4000 4200]
    sr = sampleRate
    local = localIn' 2 AR
    wn = line KR 0 1 0.1 DoNothing * whiteNoise 'θ' AR * 0.03
    ph = delTapWr b (wn + local)
    fb = delTapRd b ph del 1
    p_fb = mix (pan2 fb loc 1)
    h_fb = hpf p_fb hpp
    ao = X.averageOutput (abs h_fb) (impulse KR (recip (avg / sr)) 0) -- RFWUGens
    n_fb = h_fb * (0.02 / clip (lag ao (sdt / sr)) 0.0001 1)
    l_fb = lpf n_fb lpp
in mrg [localOut l_fb,out 0 l_fb]
