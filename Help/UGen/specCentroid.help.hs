-- specCentroid ; as the number of harmonics increases, the centroid is pushed higher
let f0 = mouseY kr 1000 100 Exponential 0.2
    nh = mouseX kr 1 100 Exponential 0.2
    z = blip ar f0 nh
    f = fft' (localBufId 'α' 2048 1) z
    c = specCentroid kr f
    p = poll' (impulse kr 1 0) c 0 (label "c")
in sinOsc ar p 0 * 0.1
