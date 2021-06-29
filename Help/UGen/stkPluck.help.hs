-- stkPluck ; default param
let delete_when_silent s a t = mrg2 s (detectSilence s a t RemoveSynth)
in delete_when_silent (X.stkPluck ar 440 0.99) 0.001 0.5

-- stkPluck ; rand param ; texture=spawn,1,inf
let freq = midiCPS (iRand 'α' 32 96)
    decay_ = rand 'β' 0.95 0.99
    s = leakDC (X.stkPluck ar freq decay_) 0.995
in mrg2 s (detectSilence s 0.001 0.5 RemoveSynth)
