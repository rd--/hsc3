-- https://www.listarc.bham.ac.uk/lists/sc-users/msg21360.html
let k_sr = 48000
    b = mce2 (localBuf 1 k_sr) (localBuf 1 k_sr)
    del = mce [0.2,0.3,0.4,0.5] * rand 0.15 0.35
    loc = mce [-1,-0.7,0.6,1]
    hpp = mce [rand 300 600,rand 900 1200]
    avg = mce [8000,14000]
    sdt = mce [300,400]
    lpp = mce [rand 4300 4500,rand 4000 4200]
    sr = sampleRate
    local = localIn' 2 ar
    wn = line kr 0 1 0.1 DoNothing * whiteNoise ar * 0.03
    ph = delTapWr b (wn + local)
    fb = delTapRd b ph del 1
    p_fb = mix (pan2 fb loc 1)
    h_fb = hpf p_fb hpp
    ao = X.averageOutput (abs h_fb) (impulse kr (recip (avg / sr)) 0) -- RFWUGens
    n_fb = h_fb * (0.02 / clip (lag ao (sdt / sr)) 0.0001 1)
    l_fb = lpf n_fb lpp
in mrg [localOut l_fb,out 0 l_fb]

-- https://www.listarc.bham.ac.uk/lists/sc-users/msg21360.html ; id
let k_sr = 48000
    b = mce2 (localBufId 'α' 1 k_sr) (localBufId 'β' 1 k_sr)
    del = mce [0.2,0.3,0.4,0.5] * randId 'γ' 0.15 0.35
    loc = mce [-1,-0.7,0.6,1]
    hpp = mce [randId 'δ' 300 600,randId 'ε' 900 1200]
    avg = mce [8000,14000]
    sdt = mce [300,400]
    lpp = mce [randId 'ζ' 4300 4500,randId 'η' 4000 4200]
    sr = sampleRate
    local = localIn' 2 ar
    wn = line kr 0 1 0.1 DoNothing * whiteNoiseId 'θ' ar * 0.03
    ph = delTapWr b (wn + local)
    fb = delTapRd b ph del 1
    p_fb = mix (pan2 fb loc 1)
    h_fb = hpf p_fb hpp
    ao = X.averageOutput (abs h_fb) (impulse kr (recip (avg / sr)) 0) -- RFWUGens
    n_fb = h_fb * (0.02 / clip (lag ao (sdt / sr)) 0.0001 1)
    l_fb = lpf n_fb lpp
in mrg [localOut l_fb,out 0 l_fb]
