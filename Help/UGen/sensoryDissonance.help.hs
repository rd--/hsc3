-- sensoryDissonance ; note 'norm' input calculation
let sig = soundIn 0
    chn = fft' (localBuf 'α' 1 2048) sig
    maxpeaks = 100
    dissonance = X.sensoryDissonance KR chn 100 0.1 (0.01 / maxpeaks) 1
in pan2 (blip AR 100 (sqrt dissonance * 200)) 0 0.1
