-- pv_SpectralMap
let y = mouseY KR (-1) 1 Linear 0.2
    x = mouseX KR (-1) 1 Linear 0.2
    b = control KR "buf" 0
    c1 = fft' (localBuf 'α' 2048 1) (soundIn 0)
    c2 = fft' (localBuf 'β' 2048 1) (playBuf 1 AR b 1 1 0 Loop DoNothing)
    c3 = X.pv_SpectralMap c1 c2 0.0 y x 1 0
in ifft' c3

-- pv_SpectralMap ; https://scsynth.org/t/share-your-delay-reverb-designs/2206/6
let freeze = control KR "freeze" 0
    mode = control KR "mode" 1
    roomsize = control KR "roomsize" 10
    revtime = control KR "revtime" 6
    c1 = fft' (localBuf 'α' 2048 1) (soundIn 0)
    c2 = fft' (localBuf 'β' 2048 1) (soundIn 1)
    c3 = X.pv_SpectralMap c1 c2 0.0 freeze mode 1 0
in gVerb (ifft' c3) roomsize revtime 0.5 0.5 15 1 0.7 0.5 300

---- ; load sndfile
withSC3 (async (b_allocRead 0 "/home/rohan/data/audio/metal.wav" 0 0))
