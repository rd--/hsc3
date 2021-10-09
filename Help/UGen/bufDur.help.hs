-- bufDur ; requires=buf ; read without loop, trigger reset based on buffer duration
let b = control kr "buf" 0
    t = impulse ar (recip (bufDur kr b)) 0
    p = sweep t (bufSampleRate kr b)
in bufRd 1 ar b p NoLoop LinearInterpolation * 0.1

-- bufDur ; requires=buf ; bufDur = bufFrames / bufSampleRate
let b = control kr "buf" 0
    t = impulse ar (bufSampleRate kr b / bufFrames kr b) 0
    p = sweep t (bufSampleRate kr b)
in bufRd 1 ar b p NoLoop LinearInterpolation * 0.1
