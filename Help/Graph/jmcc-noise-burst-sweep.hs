-- noise burst sweep (jmcc) #6 ; texture=overlap,4,2,4,inf
let n = mce2 (whiteNoise 'α' AR) (whiteNoise 'β' AR)
    lfoRate = rand 'γ' (-1) 1 + mouseX KR 10 60 Exponential 0.2
    amp = max 0 (lfSaw KR lfoRate (-1))
    cfreq = mouseY KR 400 8000 Exponential 0.2
    freq = sinOsc KR 0.2 0 * cfreq + (1.05 * cfreq)
in resonz (n * amp) freq 0.1
