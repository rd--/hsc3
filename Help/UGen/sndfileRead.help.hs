---- sndfileRead ; buffer identifier with "brackets" to hold b_allocRead and b_free messages
let (buf, nc, _, _) = sndfileRead (0, "buf") "metal.wav" -- metal.wav is mono ; pf-c5.aif is stereo
    tr = impulse ar (bufSampleRate kr buf / bufFrames kr buf) 0
    ph = phasor ar tr (bufSampleRate kr buf / sampleRate) 0 (bufFrames kr buf) 0
in bufRdL nc ar buf ph NoLoop

---- sndfileRead ; as above but use constants for sample rate and frame count
let (buf, nc, sr, nf) = sndfileRead (0, "buf") "pf-c5.aif" -- metal.wav is mono ; pf-c5.aif is stereo
    tr = impulse ar (sr / nf) 0
    ph = phasor ar tr (sr / sampleRate) 0 nf 0
in bufRdL nc ar buf ph NoLoop

---- ; print scsynth, the interpreter value that holds the reference that stores the end brackets
scsynthPrint scsynth
