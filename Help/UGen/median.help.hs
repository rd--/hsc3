-- median ; signal with impulse noise
let n = dust2Id 'α' ar 100
in median 3 (saw ar 500 * 0.1 + n * 0.9)

-- median ; length can be increased for longer duration noise
let n = dust2Id 'α' ar 100
in median 5 (saw ar 500 * 0.1 + lpz1 (n * 0.9))

-- median ; long filters begin chopping off the peaks of the waveform
let x = sinOsc ar 1000 0 * 0.2
in mce [x, median 31 x]

-- median ; median = high frequency noise ; leakDC = low frequency noise
let n = whiteNoiseId 'α' ar
    s = median 31 (n * 0.1 + sinOsc ar 800 0 * 0.1)
in leakDC s 0.9
