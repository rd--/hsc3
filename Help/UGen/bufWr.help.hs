-- bufWr
let b = localBuf 'α' 1 (sampleRate * 2)
    o = sinOsc AR (lfNoise1 'β' KR 2 * 300 + 400) 0 * 0.1
    ph z = phasor AR 0 (bufRateScale KR b * z) 0 (bufFrames KR b) 0
    w = bufWr b (ph (mouseX KR 0.25 1 Linear 0.2)) Loop o
    r = bufRdL 1 AR b (ph (mouseY KR 0.25 16 Linear 0.2)) Loop
in mrg2 r w

-- bufWr
let wr_rt = control KR "wr-rate" 1
    rd_rt = control KR "rd-rate" 1
    b = localBuf 'α' 1 (sampleRate * 2)
    o = sinOsc AR (lfNoise1 'β' KR 2 * 300 + 400) 0 * 0.1
    w = bufWr b (phasor AR 0 (bufRateScale KR b * wr_rt) 0 (bufFrames KR b) 0) Loop o
in bufRdL 1 AR b (phasor AR 0 (bufRateScale KR b * rd_rt) 0 (bufFrames KR b) 0) Loop

---- ; set read & write rates independently
withSC3 (Sound.OSC.sendMessage (n_set1 1 "wr-rate" 0.5))
withSC3 (Sound.OSC.sendMessage (n_set1 1 "rd-rate" 13.0))
