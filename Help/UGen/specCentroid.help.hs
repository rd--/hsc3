-- specCentroid ; as the number of harmonics increases, the centroid is pushed higher
let f0 = mouseY KR 1000 100 Exponential 0.2
    nh = mouseX KR 1 100 Exponential 0.2
    z = blip AR f0 nh
    f = fft' (localBuf 'α' 2048 1) z
    c = specCentroid KR f
    p = poll' (impulse KR 1 0) c 0 (label "c")
in sinOsc AR p 0 * 0.1
