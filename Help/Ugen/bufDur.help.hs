-- bufDur ; requires=buf ; read without loop, trigger reset based on buffer duration
let (b, nc) = (control kr "buf" 100, 2)
    t = impulse ar (recip (bufDur kr b)) 0
    p = sweep ar t (bufSampleRate kr b)
in bufRd nc ar b p NoLoop LinearInterpolation * 0.1

-- bufDur ; requires=buf ; bufDur = bufFrames / bufSampleRate
let (b, nc) = (control kr "buf" 100, 2)
    t = impulse ar (bufSampleRate kr b / bufFrames kr b) 0
    p = sweep ar t (bufSampleRate kr b)
in bufRd nc ar b p NoLoop LinearInterpolation * 0.1

---- ; buffer setup
withSc3 (async (b_allocRead 0 (sfResolve "pf-c5.aif") 0 0))
