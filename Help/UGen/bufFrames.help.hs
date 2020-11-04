-- bufFrames ; requires=buf ; read without loop, trigger reset based on buffer duration
let b = control KR "buf" 0
    p = phasor AR 0 (bufRateScale KR b) 0 (bufFrames KR b) 0
in bufRdL 1 AR b p NoLoop * 0.1

-- bufFrames ; mouse location drags play head
let b = control KR "buf" 0
    r = mce [0.05,0.075 .. 0.15]
    p = k2a (mouseX KR 0 (bufFrames KR b) Linear r)
in mix (bufRdL 1 AR b p NoLoop) * 0.1
