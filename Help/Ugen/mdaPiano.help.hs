-- mdaPiano ; trigger notes, three voices
let trigFreq = mce3 2 3 5
    freq = midiCps (lfNoise0Id 'α' kr trigFreq * 25 + 48) -- random note (internal rounding)
    gate = lfPulse kr trigFreq 0 0.5
    vel = lfPar kr (mce3 0.1 0.2 0.3) 0 * 35 + 55 -- varying velocity (0-127)
    hard = tRand 0.2 0.8 gate
    stereo = 0.5
    random = tRand 0 0.35 gate -- randomness of tuning
in mix (X.mdaPiano ar freq gate vel 0.8 hard 0.8 0.8 0.8 0.8 0.8 stereo 0.5 random 0.1 0) * 0.1
