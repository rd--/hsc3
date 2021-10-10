-- bufFrames ; requires=buf ; read without loop, trigger reset based on buffer duration
let (b, nc) = (control kr "buf" 0, 2)
    p = phasor ar 0 (bufRateScale kr b) 0 (bufFrames kr b) 0
in bufRdL nc ar b p NoLoop * 0.1

-- bufFrames ; requires=buf ; mouse location drags play head
let (b, nc) = (control kr "buf" 0, 2)
    r = mce [0.05, 0.075 .. 0.15]
    p = k2a (mouseX kr 0 (bufFrames kr b) Linear r)
in mix (bufRdL nc ar b p NoLoop) * 0.1

---- ; buffer setup
withSC3 (async (b_allocRead 0 (sfResolve "pf-c5.aif") 0 0))
