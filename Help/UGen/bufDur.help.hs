-- bufDur ; requires=buf ; read without loop, trigger reset based on buffer duration
let b = control KR "buf" 0
    t = impulse AR (recip (bufDur KR b)) 0
    p = sweep t (bufSampleRate KR b)
in bufRd 1 AR b p NoLoop LinearInterpolation * 0.1
