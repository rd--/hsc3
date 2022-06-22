-- sensoryDissonance ; noteId 'norm' input calculation
let sig = soundIn 0
    chn = fft' (localBufId 'α' 1 2048) sig
    maxpeaks = 100
    dissonance = X.sensoryDissonance kr chn 100 0.1 (0.01 / maxpeaks) 1
in pan2 (blip ar 100 (sqrt dissonance * 200)) 0 0.1
