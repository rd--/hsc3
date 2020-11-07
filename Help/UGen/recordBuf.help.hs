-- recordBuf ; record for four seconds (until end of buffer)
let b = control KR "buf" 0
    o = formant AR (xLine KR 400 1000 4 DoNothing) 2000 800 * 0.125
in mrg2 o (recordBuf AR b 0 1 0 1 NoLoop 1 RemoveSynth o)

-- recordBuf ; play recorded buffer back (ie. test)
let b = control KR "buf" 0
in playBuf 1 AR b 1 1 0 NoLoop RemoveSynth

-- recordBuf ; mix second signal equally with existing signal, replay to hear
let b = control KR "buf" 0
    o = formant AR (xLine KR 200 1000 4 DoNothing) 2000 800 * 0.125
in mrg2 o (recordBuf AR b 0 0.5 0.5 1 NoLoop 1 RemoveSynth o)

---- ; setup
withSC3 (async (b_alloc 0 (48000 * 4) 1))
