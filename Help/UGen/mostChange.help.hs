-- mostChange
let n = lfNoise0Id 'α' kr 1
    x = mouseX kr 200 300 Linear 0.1
    f = mostChange (n * 400 + 900) x
in sinOsc ar f 0 * 0.1
