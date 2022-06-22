-- bufWr
let b = localBufId 'α' 1 (sampleRate * 2)
    o = sinOsc ar (lfNoise1Id 'β' kr 2 * 300 + 400) 0 * 0.1
    ph z = phasor ar 0 (bufRateScale kr b * z) 0 (bufFrames kr b) 0
    w = bufWr b (ph (mouseX kr 0.25 1 Linear 0.2)) Loop o
    r = bufRdL 1 ar b (ph (mouseY kr 0.25 16 Linear 0.2)) Loop
in mrg2 r w

-- bufWr
let wr_rt = control kr "wr-rate" 1
    rd_rt = control kr "rd-rate" 1
    b = localBufId 'α' 1 (sampleRate * 2)
    o = sinOsc ar (lfNoise1Id 'β' kr 2 * 300 + 400) 0 * 0.1
    w = bufWr b (phasor ar 0 (bufRateScale kr b * wr_rt) 0 (bufFrames kr b) 0) Loop o
in bufRdL 1 ar b (phasor ar 0 (bufRateScale kr b * rd_rt) 0 (bufFrames kr b) 0) Loop

---- ; set read & write rates independently
withSc3 (Sound.OSC.sendMessage (n_set1 1 "wr-rate" 0.5))
withSc3 (Sound.OSC.sendMessage (n_set1 1 "rd-rate" 13.0))
