---- sndfileRead ; buffer identifier with "brackets" to hold b_allocRead and b_free messages
let (buf, nc, _, _) = sndfileRead ("buf", 0, []) "metal.wav" -- metal.wav is mono ; pf-c5.aif is stereo
    tr = impulse ar (bufSampleRate kr buf / bufFrames kr buf) 0
    ph = phasor ar tr (bufSampleRate kr buf / sampleRate) 0 (bufFrames kr buf) 0
in bufRdL nc ar buf ph NoLoop

---- sndfileRead ; as above but use constants for sample rate and frame count
let (buf, nc, sr, nf) = sndfileRead ("buf", 0, []) "pf-c5.aif" -- metal.wav is mono ; pf-c5.aif is stereo
    tr = impulse ar (sr / nf) 0
    ph = phasor ar tr (sr / sampleRate) 0 nf 0
in bufRdL nc ar buf ph NoLoop

-- sndfileRead ; sine wave control of playback rate, negative rate plays backwards
let (buf, nc, _, _) = sndfileRead ("buf", 0, []) "pf-c5.aif"
    f = xLine kr 0.2 8 30 RemoveSynth
    r = fSinOsc kr f 0 * 3 + 0.6
    s = bufRateScale kr buf * r
in playBuf nc ar buf s 1 0 Loop DoNothing

---- ; print scsynth, the interpreter value that holds the reference that stores the end brackets
scsynthPrint scsynth
