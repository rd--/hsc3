-- bufRateScale ; requires=buf ; read without loop, trigger reset based on buffer duration
let b = control kr "buf" 0
    r = bufRateScale kr b
    p = phasor ar 0 r 0 (bufFrames kr b) 0
in bufRdL 1 ar b p NoLoop * 0.5

-- bufRateScale ; requires=buf ; read buffer at ~ 3/4 reported sample rate
let b = control kr "buf" 0
    r = midiRatio (-5) * bufRateScale kr b
    p = phasor ar 0 r 0 (bufFrames kr b) 0
in bufRdL 1 ar b p NoLoop * 0.5

-- bufRateScale ; requires=buf ; bufRateScale = bufSampleRate / sampleRate
let b = control kr "buf" 0
    r = midiRatio (-5) * (bufSampleRate kr b / sampleRate)
    p = phasor ar 0 r 0 (bufFrames kr b) 0
in bufRdL 1 ar b p NoLoop * 0.5

-- bufRateScale ; requires=buf ; bufRateScale = bufSampleRate * sampleDur
let b = control kr "buf" 0
    r = midiRatio (-5) * (bufSampleRate kr b * sampleDur)
    p = phasor ar 0 r 0 (bufFrames kr b) 0
in bufRdL 1 ar b p NoLoop * 0.5
