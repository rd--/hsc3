-- vibrato ; at 1 Hz, note the use of DC.ar
let v = vibrato 'α' AR (dc AR 400) 1 0.02 0 0 0.04 0.1 0 0
in sinOsc AR v 0 * 0.1

-- vibrato ; compare, k-rate freq input can be a constant
let v = vibrato 'α' KR 400 1 0.02 0 0 0.04 0.1 0 0
in sinOsc AR v 0 * 0.1

-- vibrato ; control rate and rateVariation
let x = mouseX KR 2 100 Linear 0.2
    y = mouseY KR 0 1 Linear 0.2
    v = vibrato 'α' AR (dc AR 400) x 0.1 1 1 y 0.1 0 0
in sinOsc AR v 0 * 0.1

-- vibrato ; control depth and depthVariation
let n = lfNoise1 'α' KR 1 * 3 + 7
    x = mouseX KR 0 1 Linear 0.2
    y = mouseY KR 0 1 Linear 0.2
    v = vibrato 'β' AR (dc AR 400) n x 1 1 y 0.1 0 0
in sinOsc AR v 0 * 0.1
