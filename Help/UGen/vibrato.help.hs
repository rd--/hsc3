-- vibrato ; at 1 Hz, note the use of DC.ar
let v = vibrato 'α' ar (dc ar 400) 1 0.02 0 0 0.04 0.1 0 0
in sinOsc ar v 0 * 0.1

-- vibrato ; compare, k-rate freq input can be a constant
let v = vibrato 'α' kr 400 1 0.02 0 0 0.04 0.1 0 0
in sinOsc ar v 0 * 0.1

-- vibrato ; control rate and rateVariation
let x = mouseX kr 2 100 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
    v = vibrato 'α' ar (dc ar 400) x 0.1 1 1 y 0.1 0 0
in sinOsc ar v 0 * 0.1

-- vibrato ; control depth and depthVariation
let n = lfNoise1 'α' kr 1 * 3 + 7
    x = mouseX kr 0 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
    v = vibrato 'β' ar (dc ar 400) n x 1 1 y 0.1 0 0
in sinOsc ar v 0 * 0.1

-- vibrato ; mce
let v = vibrato 'α' ar (dc ar (mce2 (mce2 411 440) (mce2 419 440.5))) 1 0.02 0 0 0.04 0.1 0 0
in mix (sinOsc ar v 0 * 0.1)
