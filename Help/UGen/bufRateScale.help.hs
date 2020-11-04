-- bufRateScale ; requires=buf ; read without loop, trigger reset based on buffer duration
let b = control KR "buf" 0
    r = bufRateScale KR b
    p = phasor AR 0 r 0 (bufFrames KR b) 0
in bufRdL 1 AR b p NoLoop * 0.5

-- bufRateScale ; requires=buf ; read buffer at ~ 3/4 reported sample rate
let b = control KR "buf" 0
    r = midiRatio (-5) * bufRateScale KR b
    p = phasor AR 0 r 0 (bufFrames KR b) 0
in bufRdL 1 AR b p NoLoop * 0.5
