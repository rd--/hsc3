-- rcd ; rotating clock divider
let freqs = mce (map (\i -> midiCps (i * 5 + 50)) [0 .. 8])
    amps = mce [1, 0.5, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2]
    tr = lfPulse ar 7 0 0.01
    rot = -2
    spread = tiRandId 'α' 0 1 (impulse kr 0.13 0)
    dv = tiRandId 'β' 0 3 (impulse kr 0.1 0)
    pulses = X.rcd tr rot 0 dv spread 0 0 0 1
    oscs = sinOsc ar freqs 0 * pulses * amps
    sig = splay (mceRotate 3 oscs) 1 0.25 0 True
in sig  + combN sig 2 (mce [2, 1]) 3 * 0.3
