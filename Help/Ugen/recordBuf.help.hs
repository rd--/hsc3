-- recordBuf ; requires=buf ; record for four seconds (until end of buffer)
let b = control kr "buf" 0
    o = formant ar (xLine kr 400 1000 4 DoNothing) 2000 800 * 0.125
in mrg2 o (recordBuf ar b 0 1 0 1 NoLoop 1 RemoveSynth o)

-- recordBuf ; requires=buf ; play recorded buffer back (ie. test)
let (b, nc) = (control kr "buf" 0, 1)
in playBuf nc ar b 1 1 0 NoLoop RemoveSynth

-- recordBuf ; mix second signal equally with existing signal, replay to hear
let b = control kr "buf" 0
    o = formant ar (xLine kr 200 1000 4 DoNothing) 2000 800 * 0.125
in mrg2 o (recordBuf ar b 0 0.5 0.5 1 NoLoop 1 RemoveSynth o)

-- recordBuf ; mce
let b = control kr "buf" 0
    o = sinOsc ar (mce2 110 220) 0 * 0.1
in mrg2 o (recordBuf ar (mce2 b b) 0 0.5 0.5 1 NoLoop 1 RemoveSynth (mce1 o))

---- ; setup
withSc3 (async (b_alloc 0 (48000 * 4) 1))
